{-# LANGUAGE CPP                #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE TemplateHaskell    #-}
module Main (main) where

import Prelude        ()
import Prelude.Compat as Prelude

import Control.Lens
import Control.Monad           (when)
import Control.Monad.Catch     (Exception, MonadCatch (..), MonadThrow (..))
import Data.Aeson              (FromJSON, eitherDecode)
import Data.Bifunctor          (first)
import Data.Binary.Get         (Get, isEmpty, runGet)
import Data.Binary.Orphans     (Binary (..))
import Data.Binary.Put         (runPut)
import Data.Binary.Tagged      (BinaryTagged, HasSemanticVersion,
                                HasStructuralInfo, SemanticVersion, binaryTag',
                                binaryUntag', taggedDecodeFileOrFail,
                                taggedEncodeFile)
import Data.Char               (isSpace)
import Data.Maybe              (isJust, isNothing, mapMaybe)
import Data.Tagged             (untag)
import Data.Text               (Text)
import Data.Time               (UTCTime, formatTime, getTimeZone,
                                utcToLocalTime)
import Data.Time.Locale.Compat (TimeLocale, defaultTimeLocale)
import Data.Typeable           (Typeable)
import GHC.Generics            (Generic)
import Network.HTTP.Client     (Manager, httpLbs, newManager, responseBody)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Options.Applicative
import Path                    (Abs, Dir, File, Path, Rel, mkRelDir, mkRelFile,
                                parent, parseAbsDir, parseRelDir, parseRelFile,
                                toFilePath, (</>))
import System.Directory        (createDirectoryIfMissing, removeFile)
import System.Environment      (lookupEnv)

import qualified Data.ByteString.Lazy as LBS
import qualified Data.HashMap.Strict  as HM
import qualified Data.List            as L
import qualified Data.Text            as T
import qualified Data.Text.IO         as T

import Chat.Flowdock.REST

stringTrim :: String -> String
stringTrim = r . ltrim . r . ltrim
  where ltrim = Prelude.dropWhile isSpace
        r = Prelude.reverse

data NoUserDirectory = NoUserDirectory
  deriving (Show, Typeable)

instance Exception NoUserDirectory

tokenRelPath :: Path Rel File
tokenRelPath = $(mkRelFile "auth-token")

settingsRelPath :: Path Rel Dir
settingsRelPath = $(mkRelDir ".flowdock-grep")

flowsRelPath :: Path Rel Dir
flowsRelPath = $(mkRelDir "flows")

usersRelPath :: Path Rel File
usersRelPath = $(mkRelFile "users")

flowsListRelPath :: Path Rel File
flowsListRelPath = $(mkRelFile "flows-list")

lookupSettingsDirectory :: IO (Path Abs Dir)
lookupSettingsDirectory = do
  home <- lookupEnv "HOME"
  user <- lookupEnv "USER"
  case (home <|> user) of
    Just dir -> (</> settingsRelPath) <$> parseAbsDir dir
    Nothing  -> throwM NoUserDirectory

readAuthToken :: Path Abs Dir -> IO (Maybe AuthToken)
readAuthToken dir = readAuthToken' `catch` onIOError Nothing
  where
    readAuthToken' = do
      contents <- Prelude.readFile (toFilePath (dir </> tokenRelPath))
      return $ Just $ AuthToken $ stringTrim contents

writeAuthToken :: Path Abs Dir -> AuthToken -> IO ()
writeAuthToken dir token = do
  let filepath = toFilePath (dir </> tokenRelPath)
  createDirectoryIfMissing True (toFilePath dir)
  Prelude.writeFile filepath (getAuthToken token)

data Opts = Opts
  { optsToken        :: AuthToken
  , optsOffline      :: Bool
  , optsIgnoreCase   :: Bool
  , optsSearchAll    :: Bool
  , optsShowDate     :: Bool
  , optsBy           :: Maybe String
  , optsOrganisation :: ParamName Organisation
  , optsFlow         :: ParamName Flow
  , optsNeedle       :: String
  }
  deriving Show

paramArgument :: Mod ArgumentFields String -> Parser (ParamName a)
paramArgument m = mkParamName <$> strArgument m

authTokenParser :: Maybe AuthToken -> Parser AuthToken
authTokenParser maybeToken =
    option (eitherReader er) (long "token" <> metavar "token" <> help "Flowdock authentication token: see https://flowdock.com/account/tokens" <> def)
  where
    def = maybe idm value maybeToken
    er = Right . AuthToken

optsParser :: Maybe AuthToken -> Parser Opts
optsParser maybeToken =
  Opts <$> authTokenParser maybeToken
       <*> switch (long "offline" <> help "Consider only already downloaded logs")
       <*> switch (short 'i' <> long "ignore-case" <> help "Perform case insensitive matching")
       <*> switch (long "all" <> help "Search over all flows")
       <*> switch (long "with-date" <> help "Show date in search results")
       <*> (optional $ strOption (long "by" <> metavar "user" <> help "Only posts by user"))
       <*> paramArgument (metavar "org" <> help "Organisation slug, check it from the web url: wwww.flowdock.com/app/ORG/FLOW/")
       <*> paramArgument (metavar "flow" <> help "Flow slug (mandatory if --all not specified)")
       <*> fmap (L.intercalate " ") (many $ strArgument (metavar "needle"))

baseMessageOptions :: Maybe MessageId -> MessageOptions
baseMessageOptions sinceId =
  defMessageOptions & msgOptEvents   .~ [EventMessage, EventComment]
                    & msgOptLimit    .~ Just 100
                    & msgOptSinceId  .~ sinceId
                    & msgOptSorting  .~ Ascending

data Row = Row
  { rowMessageId  :: MessageId
  , rowUser       :: UserId
  , _rowCreatedAt :: UTCTime
  , rowText       :: Text
  }
  deriving (Eq, Ord, Show, Generic)

instance Binary Row
instance HasStructuralInfo Row
instance HasSemanticVersion Row

messageToRow :: Message -> Maybe Row
messageToRow msg = Row (msg ^. msgId) (msg ^. msgUser) (msg ^. msgCreatedAt) <$> messageContentToRow (msg ^. msgContent)
  where messageContentToRow (MTMessage text)  = Just text
        messageContentToRow (MTComment comm)  = Just (comm ^. commentText)
        messageContentToRow _                 = Nothing

parseCachePath :: Path Abs Dir -> ParamName Organisation -> ParamName Flow -> IO (Path Abs File)
parseCachePath dir org flow =
  (\o f -> dir </> flowsRelPath </> o </> f) <$> parseRelDir (getParamName org) <*> parseRelFile (getParamName flow)

saveRows :: Path Abs File -> [Row] -> IO ()
saveRows filepath rows = do
  let bytes = runPut (mapM_ (put . binaryTag') rows)
  createDirectoryIfMissing True (toFilePath (parent filepath))
  LBS.appendFile (toFilePath filepath) bytes

readRows :: Path Abs File -> IO ([Row], Bool)
readRows filepath  = do
  contents <- LBS.readFile (toFilePath filepath)
  let g = (,) <$> many get <*>Data.Binary.Get.isEmpty :: Get ([BinaryTagged (SemanticVersion Row) Row], Bool)
  return $ first (fmap binaryUntag') $ runGet g contents

grepRow :: UserMap -> Bool -> Maybe Text -> Text -> Bool -> Maybe (ParamName Flow) -> [Row] -> IO (Maybe Row)
grepRow users ignoreCase by needle showDate maybeFlow rows = go rows
  where go []           = return Nothing
        go [row]        = p row >> return (Just row)
        go (row:rows')  = p row >> go rows'

        p :: Row -> IO ()
        p row = when (textMatch && nickMatch) (printRow users row showDate maybeFlow)
          where textMatch = needle `T.isInfixOf` preprocess (rowText row)
                nickMatch = maybe True (== findUserNick users (rowUser row)) by

        preprocess :: Text -> Text
        preprocess | ignoreCase = T.toLower
                   | otherwise  = id

findUserNick :: UserMap -> UserId -> Text
findUserNick users uid =
  maybe "<ghost>" (^. userNick) $ HM.lookup uid users

lookupFlows :: FlowMap -> ParamName Organisation -> [ParamName Flow]
lookupFlows flowsMap org =
  maybe [] id $ HM.lookup org flowsMap

printRow :: UserMap -> Row -> Bool -> Maybe (ParamName Flow) -> IO ()
printRow users (Row _ uid t msg) showDate maybeFlow = do
  tzone <- getTimeZone t
  let unick = findUserNick users uid
  let formatStr = if showDate then "%Y-%m-%d %H:%M:%S" else "%H:%M:%S"
  let stamp = formatTime timeLocale formatStr (utcToLocalTime tzone t)
  let prefix = maybe "" ((<> " - ") . T.pack . getParamName) maybeFlow
  T.putStrLn $ prefix <> T.pack stamp <> " <" <> unick <> "> " <> msg

timeLocale :: TimeLocale
timeLocale = defaultTimeLocale

type UserMap = HM.HashMap UserId User

mkUserMap :: [User] -> UserMap
mkUserMap = HM.fromList . fmap f
  where f u = (u ^. userId, u)

-- | Fetch users from API
fetchUsers :: AuthToken -> IO UserMap
fetchUsers token = do
  mgr <- newManager tlsManagerSettings
  req <- untag <$> usersRequest
  let req' = authenticateRequest token req
  res <- httpLbs req' mgr
  users <- throwDecode (responseBody res)
  return $ mkUserMap users

fetchFlowMap :: AuthToken -> IO FlowMap
fetchFlowMap token = do
   mgr <- newManager tlsManagerSettings
   req <- untag <$> flowsRequest
   let req' = authenticateRequest token req
   res <- httpLbs req' mgr
   flows <- throwDecode (responseBody res)
   return $ mkFlowMap flows

type FlowMap = HM.HashMap (ParamName Organisation) [ParamName Flow]

mkFlowMap :: [Flow] -> FlowMap
mkFlowMap flows =
  let grouped = L.groupBy (\a b -> _flowOrganisation a == _flowOrganisation b) flows
      organisations = L.map (_foParamName . _flowOrganisation . Prelude.head) grouped
      flowNames = L.map (\groupFlows -> L.map _flowParamName groupFlows) grouped
  in  HM.fromList (organisations `L.zip` flowNames)

readFlows :: Path Abs Dir -> AuthToken -> Bool -> IO FlowMap
readFlows settingsDirectory token offline = do
  let filepath = settingsDirectory </> flowsListRelPath
  let readCached = do eFlows <- taggedDecodeFileOrFail (toFilePath filepath)
                      case eFlows of
                        Left (_, err) -> do Prelude.putStrLn $ "Error: corrupted flow list file: " <> err <> "; removing..."
                                            removeFile (toFilePath filepath)
                                            return HM.empty
                        Right x  -> return x
  let fetchFlowMap' = do flows <- fetchFlowMap token
                         taggedEncodeFile (toFilePath filepath) flows
                         return flows
  if offline
     then readCached `catch` onIOError HM.empty
     else readCached `catch` withIOError (const fetchFlowMap')

readUsers :: Path Abs Dir -> AuthToken -> Bool -> IO UserMap
readUsers settingsDirectory token offline = do
  let filepath = settingsDirectory </> usersRelPath
  let readCached = do eUsers <- taggedDecodeFileOrFail (toFilePath filepath)
                      case eUsers of
                        Left (_, err) -> do Prelude.putStrLn $ "Error: corrupted user file: " <> err <> "; removing..."
                                            removeFile (toFilePath filepath)
                                            return HM.empty
                        Right x  -> return x
  let fetchUsers' = do users <- fetchUsers token
                       taggedEncodeFile (toFilePath filepath) users
                       return users
  if offline
     then readCached `catch` onIOError HM.empty
     else readCached `catch` withIOError (const fetchUsers')

optsNeedle' :: Opts -> Text
optsNeedle' opts = if optsIgnoreCase opts
                      then T.toLower needle
                      else needle
  where needle = T.pack $ optsNeedle opts

main' :: Path Abs Dir -> Bool -> Opts -> IO ()
main' settingsDirectory writeToken opts = do
  let token = optsToken opts
  let org = optsOrganisation opts
  let flow = optsFlow opts
  let ignoreCase = optsIgnoreCase opts
  let by = T.pack <$> optsBy opts
  let searchAll = optsSearchAll opts
  let showDate = optsShowDate opts
  let needle = optsNeedle' opts

  -- Save auth token
  when writeToken $ writeAuthToken settingsDirectory token

  -- Users
  users <- readUsers settingsDirectory token (optsOffline opts)

  flowsMap <- readFlows settingsDirectory token (optsOffline opts)
  let flows = lookupFlows flowsMap org

  case (flow `Prelude.elem` flows, searchAll) of
    (False, False)  -> error $ "You are not a member of flow: " <> getParamName flow <> " in organisation: " <> getParamName org
    (True , False)  -> doFlow token org ignoreCase by needle users showDate False flow
    _               -> let needle' = (T.pack . getParamName $ flow) <> " " <> needle
                       in mapM_ (doFlow token org ignoreCase by needle' users showDate True) flows
  where
    doFlow token org ignoreCase by needle users showDate showFlow aFlow = do
      let maybeFlow = if showFlow then Just aFlow else Nothing

      -- Cache file
      cachePath <- parseCachePath settingsDirectory org aFlow

      -- Read from cache
      (rows, allRead) <- readRows cachePath `catch` onIOError ([], True)
      lastRow <- grepRow users ignoreCase by needle showDate maybeFlow rows
      when (not allRead) $ Prelude.putStrLn "Error: corrupted cache file, removing..." >> removeFile (toFilePath cachePath)

      -- Read from API
      when (not $ optsOffline opts) $ do
        mgr <- newManager tlsManagerSettings
        onlineLoop mgr token org aFlow cachePath users ignoreCase by needle showDate maybeFlow lastRow


onlineLoop :: Manager -> AuthToken -> ParamName Organisation -> ParamName Flow -> Path Abs File -> UserMap -> Bool -> Maybe Text -> Text -> Bool -> Maybe (ParamName Flow) -> Maybe Row -> IO ()
onlineLoop mgr token org flow cachePath users ignoreCase by needle showDate maybeFlow = go
  where go lastRow = do req <- untag <$> messagesRequest org flow (baseMessageOptions $ rowMessageId <$> lastRow)
                        let req' = authenticateRequest token req
                        res <- httpLbs req' mgr
                        rows <- mapMaybe messageToRow <$> throwDecode (responseBody res) :: IO [Row]
                        saveRows cachePath rows
                        lastRow' <- grepRow users ignoreCase by needle showDate maybeFlow rows
                        -- Loop only if we got something
                        when (isJust lastRow') $ go lastRow'

main :: IO ()
main = do
  settingsDirectory <- lookupSettingsDirectory
  token <- readAuthToken settingsDirectory
  let opts = info (helper <*> optsParser token) (fullDesc <> progDesc "Try --help if unsure" <> header "flowdock-grep - grep flowdock logs")
  parsedOpts <- execParser opts
  main' settingsDirectory (isNothing token) parsedOpts

-- Helpers
throwDecode :: (MonadThrow m, FromJSON a) => LBS.ByteString -> m a
throwDecode bs = case eitherDecode bs of
  Right x   -> return x
  Left err  -> error $ "throwDecode: " <> err

onIOError :: a -> IOError -> IO a
onIOError x _ = return x

withIOError :: (IOError -> a) -> IOError -> a
withIOError = id
