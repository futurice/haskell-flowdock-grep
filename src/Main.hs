{-# LANGUAGE CPP                #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE TemplateHaskell    #-}
module Main (main) where

import Prelude ()
import Prelude.Compat as Prelude

import Control.Lens
import Control.Monad           (when)
import Control.Monad.Catch     (Exception, MonadCatch (..), MonadThrow (..))
import Data.Aeson              (FromJSON, eitherDecode)
import Data.Bifunctor          (first)
import Data.Binary.Get         (Get, isEmpty, runGet)
import Data.Binary.Orphans     (Binary (..))
import Data.Binary.Put         (runPut)
import Data.Binary.Tagged
       (BinaryTagged, HasSemanticVersion, HasStructuralInfo, SemanticVersion,
       binaryTag', binaryUntag', taggedDecodeFileOrFail, taggedEncodeFile)
import Data.Char               (isSpace)
import Data.Foldable           (toList, traverse_)
import Data.Function           (on)
import Data.Hashable           (hash)
import Data.Maybe              (isJust, isNothing, mapMaybe)
import Data.Monoid             ((<>))
import Data.Tagged             (Tagged, untag)
import Data.Text               (Text)
import Data.Monoid
import Data.Tagged             (Tagged, untag)
import Data.Text               (Text)
import Data.Text.Lens          (packed)
import Data.Time
       (UTCTime, formatTime, getTimeZone, utcToLocalTime)
import Data.Time.Locale.Compat (TimeLocale, defaultTimeLocale)
import Data.Typeable           (Typeable)
import Data.Vector             (Vector)
import GHC.Generics            (Generic)
import Network.HTTP.Client
       (Manager, Request, httpLbs, newManager, responseBody)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Options.Applicative
import Path
       (Abs, Dir, File, Path, Rel, mkRelDir, mkRelFile, parent, parseAbsDir,
       parseRelDir, parseRelFile, toFilePath, (</>))
import System.Directory        (createDirectoryIfMissing, removeFile)
import System.Environment      (lookupEnv)

import qualified Data.ByteString.Lazy as LBS
import qualified Data.HashMap.Strict  as HM
import qualified Data.List            as L
import qualified Data.List.NonEmpty   as NE
import qualified Data.Text            as T
import qualified Data.Text.IO         as T
import qualified Data.Vector          as V
import qualified System.Console.ANSI  as ANSI

import Chat.Flowdock.REST
import Chat.Flowdock.REST.Internal (getIdentifier)

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
  , optsShowDate     :: Bool
  , optsShowMsgUrl   :: Bool
  , optsBy           :: Maybe String
  , optsOrganisation :: ParamName Organisation
  , optsFlow         :: Maybe (ParamName Flow)
  , optsNeedle       :: String
  }
  deriving Show

paramArgument :: Mod ArgumentFields String -> Parser (ParamName a)
paramArgument m = mkParamName <$> strArgument m

authTokenParser :: Maybe AuthToken -> Parser AuthToken
authTokenParser maybeToken =
    option (eitherReader er) (short 't' <> long "token" <> metavar "token" <> help "Flowdock authentication token: see https://flowdock.com/account/tokens" <> def)
  where
    def = maybe idm value maybeToken
    er = Right . AuthToken

optsParser :: Maybe AuthToken -> Parser Opts
optsParser maybeToken =
  Opts <$> authTokenParser maybeToken
       <*> switch (short 'o' <> long "offline"     <> help "Consider only already downloaded logs")
       <*> switch (short 'i' <> long "ignore-case" <> help "Perform case insensitive matching")
       <*> switch (short 'd' <> long "show-date"   <> help "Show date in search results")
       <*> switch (short 'u' <> long "show-url"    <> help "Show message URL in search results")
       <*> (optional $ strOption (short 'u' <> long "by" <> metavar "user" <> help "Only posts by user"))
       <*> paramArgument (metavar "org" <> help "Organisation slug, check it from the web url: wwww.flowdock.com/app/ORG/FLOW/")
       <*> flowParser
       <*> fmap (L.intercalate " ") (many $ strArgument (metavar "needle"))

flowParser :: Parser (Maybe (ParamName Flow))
flowParser = Nothing <$ allParser <|> Just <$> flowNameParser
  where
    allParser = flag' () (short 'a' <> long "all" <> help "Search over all flows")
    flowNameParser = paramArgument (metavar "flow" <> help "Flow slug (mandatory if --all not specified)")

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
  , rowTags      :: !(Vector Tag)
  , rowText       :: Text
  }
  deriving (Eq, Ord, Show, Generic)

instance Binary Row
instance HasStructuralInfo Row
instance HasSemanticVersion Row

-- | Row text with appendet tags
rowText' :: Row -> Text
rowText' row = rowText row <> foldMap ((" #" <>) . getTag) (rowTags row)

messageToRow :: Message -> Maybe Row
messageToRow msg = Row
    (msg ^. msgId)
    (msg ^. msgUser)
    (msg ^. msgCreatedAt)
    (filterTags $ msg ^. msgTags)
    <$> messageContentToRow (msg ^. msgContent)
  where
    messageContentToRow (MTMessage text)  = Just text
    messageContentToRow (MTComment comm)  = Just (comm ^. commentText)
    messageContentToRow _                 = Nothing

    filterTags = V.filter (not . tagPredicate . getTag)
    tagPredicate t =
        T.isPrefixOf ":" t
        || T.isPrefixOf "influx:" t

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

grepRow :: UserMap -> Bool -> Maybe Text -> Text -> Bool -> Maybe (ParamName Flow) -> ParamName Organisation -> Bool -> [Row] -> IO (Maybe Row)
grepRow users ignoreCase by needle showDate maybeFlow org showMsgUrl rows = go rows
  where go []           = return Nothing
        go [row]        = p row >> return (Just row)
        go (row:rows')  = p row >> go rows'

        p :: Row -> IO ()
        p row = when (textMatch && nickMatch) (printRow users row showDate maybeFlow org showMsgUrl)
          where textMatch = needle `T.isInfixOf` preprocess (rowText' row)
                nickMatch = maybe True (== findUserNick users (rowUser row)) by

        preprocess :: Text -> Text
        preprocess | ignoreCase = T.toLower
                   | otherwise  = id

findUserNick :: UserMap -> UserId -> Text
findUserNick users uid =
    maybe ("<" <> uid' <> ">") (^. userNick) $ HM.lookup uid users
  where
    uid' = show (getIdentifier uid) ^. packed

lookupFlows :: FlowMap -> ParamName Organisation -> [ParamName Flow]
lookupFlows flowsMap org =
  maybe [] NE.toList $ HM.lookup org flowsMap

printRow :: UserMap -> Row -> Bool -> Maybe (ParamName Flow) -> ParamName Organisation -> Bool -> IO ()
printRow users (Row rowId uid t tags msg) showDate maybeFlow org showMsgUrl = do
    tzone <- getTimeZone t
    let unick = findUserNick users uid
    let nickColor = nickColors !! (hash unick `mod` length nickColors)
    let formatStr = if showDate then "%Y-%m-%d %H:%M:%S" else "%H:%M:%S"
    let stamp = formatTime timeLocale formatStr (utcToLocalTime tzone t)
    let prefix = if showMsgUrl     then "" else maybe "" ((<> " ") . T.pack . getParamName) maybeFlow
    let msgUrl = if not showMsgUrl then "" else
          maybe "" (\flow ->
                      "https://flowdock.com/app/"
                      <> T.pack (getParamName org)
                      <> "/"
                      <> T.pack (getParamName flow)
                      <> "/messages/"
                      <> T.pack (show (getIdentifier rowId))
                     <> " "
                   ) maybeFlow
    let tags' | null tags = ""
              | otherwise = foldMap ((" #" <>) . getTag) $ toList tags
    T.putStr $ prefix <> T.pack stamp <> " <"
    ANSI.setSGR [uncurry (ANSI.SetColor ANSI.Foreground) nickColor]
    T.putStr unick
    ANSI.setSGR [ANSI.Reset]
    T.putStr $ "> " <> msg
    ANSI.setSGR [ANSI.SetColor ANSI.Foreground ANSI.Dull ANSI.White]
    T.putStr tags'
    ANSI.setSGR [ANSI.Reset]
    T.putStrLn msgUrl
  where
    nickColors =
        [ (ANSI.Dull, ANSI.Red)
        , (ANSI.Dull, ANSI.Green)
        , (ANSI.Dull, ANSI.Yellow)
        , (ANSI.Dull, ANSI.Blue)
        , (ANSI.Dull, ANSI.Cyan)
        , (ANSI.Dull, ANSI.Magenta)
        ]

timeLocale :: TimeLocale
timeLocale = defaultTimeLocale

type UserMap = HM.HashMap UserId User

mkUserMap :: [User] -> UserMap
mkUserMap = HM.fromList . fmap f
  where f u = (u ^. userId, u)

fetch :: FromJSON t => AuthToken -> IO (Tagged s Request) -> (t -> b) -> IO b
fetch token request parse = do
  mgr <- newManager tlsManagerSettings
  req <- untag <$> request
  let req' = authenticateRequest token req
  res <- httpLbs req' mgr
  results <- throwDecode (responseBody res)
  return $ parse results

-- | Fetch users from API
fetchUsers :: AuthToken -> IO UserMap
fetchUsers token = fetch token usersRequest mkUserMap

-- | Fetch joined flows from API
fetchFlowMap :: AuthToken -> IO FlowMap
fetchFlowMap token = fetch token flowsRequest mkFlowMap

type FlowMap = HM.HashMap (ParamName Organisation) (NE.NonEmpty (ParamName Flow))

mkFlowMap :: [Flow] -> FlowMap
mkFlowMap flows = HM.fromList (L.map makePair grouped)
  where
    grouped = NE.groupBy ((==) `on` _flowOrganisation) flows
    makePair group' =
        ( _foParamName . _flowOrganisation . NE.head $ group'
        , NE.map _flowParamName group'
        )

readCached
    :: (Binary a, HasSemanticVersion a, HasStructuralInfo a)
    => String -> (AuthToken -> IO a) -> Path Rel File -> a -> Path Abs Dir -> AuthToken -> Bool -> IO a
readCached msg fetcher filePath empty' settingsDirectory token offline = do
  let filepath = settingsDirectory </> filePath
  let readCached' = do fromFile <- taggedDecodeFileOrFail (toFilePath filepath)
                       case fromFile of
                         Left (_, err) -> do Prelude.putStrLn $ "Error: corrupted " <> msg <> " file: " <> err <> "; removing..."
                                             removeFile (toFilePath filepath)
                                             return empty'
                         Right x  -> return x
  let fetchOnline' = do values <- fetcher token
                        taggedEncodeFile (toFilePath filepath) values
                        return values
  if offline
     then readCached' `catch` onIOError empty'
     else readCached' `catch` withIOError (const fetchOnline')

readFlows :: Path Abs Dir -> AuthToken -> Bool -> IO FlowMap
readFlows = readCached "flow list" fetchFlowMap flowsListRelPath HM.empty

readUsers :: Path Abs Dir -> AuthToken -> Bool -> IO UserMap
readUsers = readCached "user" fetchUsers usersRelPath HM.empty

optsNeedle' :: Opts -> Text
optsNeedle' opts =
    if optsIgnoreCase opts
        then T.toLower needleArg
        else needleArg
  where
    needleArg = T.pack $ optsNeedle opts

main' :: Path Abs Dir -> Bool -> Opts -> IO ()
main' settingsDirectory writeToken opts = do
  let token = optsToken opts
  let org = optsOrganisation opts
  let ignoreCase = optsIgnoreCase opts
  let by = T.pack <$> optsBy opts
  let showDate = optsShowDate opts
  let needle = optsNeedle' opts
  let showMsgUrl = optsShowMsgUrl opts

  -- Save auth token
  when writeToken $ writeAuthToken settingsDirectory token

  -- Users
  users <- readUsers settingsDirectory token (optsOffline opts)

  flowsMap <- readFlows settingsDirectory token (optsOffline opts)
  let flows = lookupFlows flowsMap org

  case optsFlow opts of
    Nothing -> traverse_ (doFlow token org ignoreCase by needle users showDate True showMsgUrl) flows
    Just flow
      | flow `elem` flows -> doFlow token org ignoreCase by needle users showDate False showMsgUrl flow
      | otherwise
          -> error $ "You are not a member of flow: " <> getParamName flow <> " in organisation: " <> getParamName org
  where
    doFlow token org ignoreCase by needle users showDate showFlow showMsgUrl aFlow = do
      let maybeFlow = if showFlow then Just aFlow else Nothing

      -- Cache file
      cachePath <- parseCachePath settingsDirectory org aFlow

      -- Read from cache
      (rows, allRead) <- readRows cachePath `catch` onIOError ([], True)
      lastRow <- grepRow users ignoreCase by needle showDate maybeFlow org showMsgUrl rows
      when (not allRead) $ Prelude.putStrLn "Error: corrupted cache file, removing..." >> removeFile (toFilePath cachePath)

      -- Read from API
      when (not $ optsOffline opts) $ do
        mgr <- newManager tlsManagerSettings
        onlineLoop mgr token org aFlow cachePath users ignoreCase by needle showDate maybeFlow showMsgUrl lastRow


onlineLoop :: Manager -> AuthToken -> ParamName Organisation -> ParamName Flow -> Path Abs File -> UserMap -> Bool -> Maybe Text -> Text -> Bool -> Maybe (ParamName Flow) -> Bool -> Maybe Row -> IO ()
onlineLoop mgr token org flow cachePath users ignoreCase by needle showDate maybeFlow showMsgUrl = go
  where go lastRow = do req <- untag <$> messagesRequest org flow (baseMessageOptions $ rowMessageId <$> lastRow)
                        let req' = authenticateRequest token req
                        res <- httpLbs req' mgr
                        rows <- mapMaybe messageToRow <$> throwDecode (responseBody res) :: IO [Row]
                        saveRows cachePath rows
                        lastRow' <- grepRow users ignoreCase by needle showDate maybeFlow org showMsgUrl rows
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
