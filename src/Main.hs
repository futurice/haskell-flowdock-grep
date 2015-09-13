{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Main (main) where

import Control.Lens
import Control.Monad
import Control.Monad.Catch
import Data.Aeson
import Data.Bifunctor
import Data.Binary.Put
import Data.Binary.Get
import Data.Binary.Orphans
import Data.Binary.Tagged
import Data.ByteString.Lazy as LBS
import Data.Char
import Data.HashMap.Strict as HM
import Data.List as L
import Data.Maybe (isNothing, isJust, mapMaybe)
import Data.Monoid
import Data.Tagged
import Data.Text as T
import Data.Text.IO as T
import Data.Time
import qualified Data.Foldable  as F (foldr)
#if !MIN_VERSION_time(1,5,0)
import System.Locale
#endif
import Data.Typeable
import GHC.Generics
import Network.HTTP.Client
import Network.HTTP.Client.TLS
import Options.Applicative
import Path
import System.Directory
import System.Environment

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
  { optsToken :: AuthToken
  , optsOffline :: Bool
  , optsIgnoreCase :: Bool
  , optsBy :: Maybe String
  , optsOrganisation :: ParamName Organisation
  , optsFlow :: ParamName Flow
  , optsNeedle :: String
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
       <*> (optional $ strOption (long "by" <> metavar "USER" <> help "Only posts by USER"))
       <*> paramArgument (metavar "org" <> help "Organisation slug, check it from the web url: wwww.flowdock.com/app/ORG/FLOW/")
       <*> paramArgument (metavar "flow" <> help "Flow slug")
       <*> fmap (L.intercalate " ") (many $ strArgument (metavar "needle"))

baseMessageOptions :: Maybe MessageId -> MessageOptions
baseMessageOptions sinceId =
  defMessageOptions & msgOptEvents   .~ [EventMessage, EventComment]
                    & msgOptLimit    .~ Just 100
                    & msgOptSinceId  .~ sinceId
                    & msgOptSorting  .~ Ascending

data Row = Row
  { rowMessageId  :: MessageId
  , _rowUser       :: UserId
  , _rowCreatedAt  :: UTCTime
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

grepRow :: UserMap -> Bool -> Maybe Text -> Text -> [Row] -> IO (Maybe Row)
grepRow users ignoreCase by needle rows = go rows
  where go []           = return Nothing
        go [row]        = p row >> return (Just row)
        go (row:rows')  = p row >> go rows'

        p :: Row -> IO ()
        p row = when (needle `T.isInfixOf` preprocess (rowText row) &&
                      F.foldr (\nick _ -> nick == findUserNick users (_rowUser row)) True by) (printRow users row)

        preprocess :: Text -> Text
        preprocess | ignoreCase = T.toLower
                   | otherwise  = id

findUserNick :: UserMap -> UserId -> Text
findUserNick users uid =
  maybe "<ghost>" (^. userNick) $ HM.lookup uid users


printRow :: UserMap -> Row -> IO ()
printRow users (Row _ uid t msg) = do
  tzone <- getTimeZone t
  let unick = findUserNick users uid
  let stamp = formatTime timeLocale "%H:%M:%S" (utcToLocalTime tzone t)
  T.putStrLn $ T.pack stamp <> " <" <> unick <> "> " <> msg

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
  let by = fmap T.pack $ optsBy opts
  let needle = optsNeedle' opts

  -- Save auth token
  when writeToken $ writeAuthToken settingsDirectory token

  -- Users
  users <- readUsers settingsDirectory token (optsOffline opts)

  -- Cache file
  cachePath <- parseCachePath settingsDirectory org flow

  -- Read from cache
  (rows, allRead) <- readRows cachePath `catch` onIOError ([], True)
  lastRow <- grepRow users ignoreCase by needle rows
  when (not allRead) $ Prelude.putStrLn "Error: corrupted cache file, removing..." >> removeFile (toFilePath cachePath)

  -- Read from API
  when (not $ optsOffline opts) $ do
    mgr <- newManager tlsManagerSettings
    onlineLoop mgr token org flow cachePath users ignoreCase by needle lastRow

onlineLoop :: Manager -> AuthToken -> ParamName Organisation -> ParamName Flow -> Path Abs File -> UserMap -> Bool -> Maybe Text -> Text -> Maybe Row -> IO ()
onlineLoop mgr token org flow cachePath users ignoreCase by needle = go
  where go lastRow = do req <- untag <$> messagesRequest org flow (baseMessageOptions $ rowMessageId <$> lastRow)
                        let req' = authenticateRequest token req
                        res <- httpLbs req' mgr
                        rows <- mapMaybe messageToRow <$> throwDecode (responseBody res) :: IO [Row]
                        saveRows cachePath rows
                        lastRow' <- grepRow users ignoreCase by needle rows
                        -- Loop only if we got something
                        when (isJust lastRow') $ go lastRow'

main :: IO ()
main = do
  settingsDirectory <- lookupSettingsDirectory
  token <- readAuthToken settingsDirectory
  let opts = info (helper <*> optsParser token) (fullDesc <> progDesc "Try --help if unsure" <> header "flowdock-grep - grep flowdock logs")
  execParser opts >>= main' settingsDirectory (isNothing token)

-- Helpers
throwDecode :: (MonadThrow m, FromJSON a) => LBS.ByteString -> m a
throwDecode bs = case eitherDecode bs of
  Right x   -> return x
  Left err  -> error $ "throwDecode: " <> err

onIOError :: a -> IOError -> IO a
onIOError x _ = return x

withIOError :: (IOError -> a) -> IOError -> a
withIOError = id
