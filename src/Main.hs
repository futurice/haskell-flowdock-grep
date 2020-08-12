{-# LANGUAGE CPP                #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE TemplateHaskell    #-}
module Main (main) where

import Control.Lens   ((&), (.~), (^.))
import Prelude ()
import Prelude.Compat as Prelude

import Control.Monad           (unless, when)
import Control.Monad.Catch     (Exception, MonadCatch (..), MonadThrow (..))
import Data.Aeson.Compat       (FromJSON, decode)
import Data.Bifunctor          (first)
import Data.Binary             (Binary (..))
import Data.Binary.Get         (Get, isEmpty, runGet)
import Data.Binary.Put         (runPut)
import Data.Binary.Tagged
       (BinaryTagged, HasSemanticVersion, HasStructuralInfo, SemanticVersion,
       binaryTag', binaryUntag', taggedDecodeFileOrFail, taggedEncodeFile)
import Data.Char               (isSpace)
import Data.Foldable           (toList, traverse_)
import Data.Function           (on)
import Data.Hashable           (hash)
import Data.Maybe              (isJust, isNothing, mapMaybe)
import Data.Semigroup          ((<>))
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

    stringTrim :: String -> String
    stringTrim = r . ltrim . r . ltrim
      where
        ltrim = Prelude.dropWhile isSpace
        r = Prelude.reverse

writeAuthToken :: Path Abs Dir -> AuthToken -> IO ()
writeAuthToken dir token = do
    let filepath = toFilePath (dir </> tokenRelPath)
    createDirectoryIfMissing True (toFilePath dir)
    Prelude.writeFile filepath (getAuthToken token)

data Opts = Opts
    { optsToken        :: !AuthToken
    , optsOffline      :: !Bool
    , optsIgnoreCase   :: !Bool
    , optsShowDate     :: !Bool
    , optsShowMsgUrl   :: !Bool
    , optsNoColors     :: !Bool
    , optsBy           :: !(Maybe Text)
    , optsOrganisation :: !(ParamName Organisation)
    , optsFlow         :: !(Maybe (ParamName Flow))
    , optsNeedle       :: !Text
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
optsParser maybeToken = Opts
    <$> authTokenParser maybeToken
    <*> switch (short 'o' <> long "offline"     <> help "Consider only already downloaded logs")
    <*> switch (short 'i' <> long "ignore-case" <> help "Perform case insensitive matching")
    <*> switch (short 'd' <> long "show-date"   <> help "Show date in search results")
    <*> switch (short 'u' <> long "show-url"    <> help "Show message URL in search results")
    <*> switch (short 'n' <> long "no-colors"   <> help "No colors")
    <*> (optional $ T.pack <$> strOption (short 'u' <> long "by" <> metavar "user" <> help "Only posts by user"))
    <*> paramArgument (metavar "org" <> help "Organisation slug, check it from the web url: wwww.flowdock.com/app/ORG/FLOW/")
    <*> flowParser
    <*> fmap (T.pack . L.intercalate " ") (many $ strArgument (metavar "needle"))

flowParser :: Parser (Maybe (ParamName Flow))
flowParser = Nothing <$ allParser <|> Just <$> flowNameParser
  where
    allParser = flag' () (short 'a' <> long "all" <> help "Search over all flows")
    flowNameParser = paramArgument (metavar "flow" <> help "Flow slug (mandatory if --all not specified)")

baseMessageOptions :: Maybe MessageId -> MessageOptions
baseMessageOptions sinceId = defaultMessageOptions
    & msgOptEvents   .~ [EventMessage, EventComment]
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
parseCachePath dir org flow = mk
    <$> parseRelDir (getParamName org)
    <*> parseRelFile (getParamName flow)
  where
    mk o f = dir </> flowsRelPath </> o </> f

saveRows :: Path Abs File -> [Row] -> IO ()
saveRows filepath rows = do
    let bytes = runPut $ traverse_ (put . binaryTag') rows
    createDirectoryIfMissing True $ toFilePath (parent filepath)
    LBS.appendFile (toFilePath filepath) bytes

readRows :: Path Abs File -> IO ([Row], Bool)
readRows filepath  = do
    contents <- LBS.readFile (toFilePath filepath)
    let g = (,) <$> many get <*> Data.Binary.Get.isEmpty :: Get ([BinaryTagged (SemanticVersion Row) Row], Bool)
    return $ first (fmap binaryUntag') $ runGet g contents

grepRow :: UserMap -> Opts -> [Row] -> IO (Maybe Row)
grepRow users opts rows = go rows
  where
    go []           = return Nothing
    go [row]        = p row >> return (Just row)
    go (row:rows')  = p row >> go rows'

    p :: Row -> IO ()
    p row = when (textMatch && nickMatch) (printRow users row opts)
      where
        textMatch = optsNeedle' opts `T.isInfixOf` preprocess (rowText' row)
        nickMatch = maybe True (== findUserNick users (rowUser row)) (optsBy opts)

    preprocess :: Text -> Text
    preprocess | optsIgnoreCase opts = T.toLower
               | otherwise           = id

findUserNick :: UserMap -> UserId -> Text
findUserNick users uid =
    maybe ("<" <> uid' <> ">") (^. userNick) $ HM.lookup uid users
  where
    uid' = show (getIdentifier uid) ^. packed

lookupFlows :: FlowMap -> ParamName Organisation -> [ParamName Flow]
lookupFlows flowsMap org =
    maybe [] toList $ HM.lookup org flowsMap

printRow :: UserMap -> Row -> Opts -> IO ()
printRow users (Row rowId uid t tags msg) opts = do
    tzone <- getTimeZone t
    let maybeFlow = optsFlow opts
    let unick     = findUserNick users uid
    let nickColor = nickColors !! (hash unick `mod` length nickColors)
    let formatStr = if optsShowDate opts then "%Y-%m-%d %H:%M:%S" else "%H:%M:%S"
    let stamp     = formatTime timeLocale formatStr (utcToLocalTime tzone t)
    let prefix    = if optsShowMsgUrl opts then "" else maybe "" ((<> " ") . T.pack . getParamName) maybeFlow
    let msgUrl    = if not (optsShowMsgUrl opts) then "" else maybe "" msgUrl' maybeFlow
    let tags' | null tags = ""
              | otherwise = foldMap ((" #" <>) . getTag) $ toList tags
    T.putStr $ prefix <> T.pack stamp <> " <"
    withColors $ ANSI.setSGR [uncurry (ANSI.SetColor ANSI.Foreground) nickColor]
    T.putStr unick
    withColors $ ANSI.setSGR [ANSI.Reset]
    T.putStr $ "> " <> msg
    withColors $ ANSI.setSGR [ANSI.SetColor ANSI.Foreground ANSI.Dull ANSI.White]
    T.putStr tags'
    T.putStrLn msgUrl
    withColors $ ANSI.setSGR [ANSI.Reset]
  where
    msgUrl' flow = mconcat
        [ " https://flowdock.com/app/"
        , T.pack (getParamName $ optsOrganisation opts)
        , "/"
        , T.pack (getParamName flow)
        , "/messages/"
        ,  T.pack (show (getIdentifier rowId))
        ]

    withColors = unless (optsNoColors opts)
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
    results <- decode (responseBody res)
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
        , _flowParamName <$> group'
        )

readCached
    :: (Binary a, HasSemanticVersion a, HasStructuralInfo a)
    => String -> (AuthToken -> IO a) -> Path Rel File -> a -> Path Abs Dir -> AuthToken -> Bool -> IO a
readCached msg fetcher filePath empty' settingsDirectory token offline
    | offline   = readCached' `catch` onIOError empty'
    | otherwise = readCached' `catch` withIOError (const fetchOnline')
  where
    filePath' = settingsDirectory </> filePath
    filePath'' = toFilePath filePath'

    readCached' = do
        fromFile <- taggedDecodeFileOrFail filePath''
        case fromFile of
            Left (_, err) -> do
                Prelude.putStrLn $ "Error: corrupted " <> msg <> " file: " <> err <> "; removing..."
                removeFile filePath''
                return empty'
            Right x  -> return x

    fetchOnline' = do
        values <- fetcher token
        taggedEncodeFile filePath'' values
        return values


readFlows :: Path Abs Dir -> AuthToken -> Bool -> IO FlowMap
readFlows = readCached "flow list" fetchFlowMap flowsListRelPath HM.empty

readUsers :: Path Abs Dir -> AuthToken -> Bool -> IO UserMap
readUsers = readCached "user" fetchUsers usersRelPath HM.empty

optsNeedle' :: Opts -> Text
optsNeedle' opts
    | optsIgnoreCase opts = T.toLower $ optsNeedle opts
    | otherwise           = optsNeedle opts

main' :: Path Abs Dir -> Bool -> Opts -> IO ()
main' settingsDirectory writeToken opts = do
    let token = optsToken opts
    let org = optsOrganisation opts

    -- Save auth token
    when writeToken $ writeAuthToken settingsDirectory token

    -- Users
    users <- readUsers settingsDirectory token (optsOffline opts)

    flowsMap <- readFlows settingsDirectory token (optsOffline opts)
    let flows = lookupFlows flowsMap org

    case optsFlow opts of
        Nothing -> traverse_ (doFlow users True) flows
        Just flow
            | flow `elem` flows -> doFlow users False flow
            | otherwise
                -> error $ "You are not a member of flow: " <> getParamName flow <> " in organisation: " <> getParamName org
  where
    doFlow :: UserMap -> Bool -> ParamName Flow -> IO ()
    doFlow users showFlow aFlow = do
      let opts' = opts { optsFlow = if showFlow || optsShowMsgUrl opts then Just aFlow else Nothing}

      -- Cache file
      cachePath <- parseCachePath settingsDirectory (optsOrganisation opts') aFlow

      -- Read from cache
      (rows, allRead) <- readRows cachePath `catch` onIOError ([], True)
      lastRow <- grepRow users opts' rows
      when (not allRead) $ Prelude.putStrLn "Error: corrupted cache file, removing..." >> removeFile (toFilePath cachePath)

      -- Read from API
      when (not $ optsOffline opts) $ do
          mgr <- newManager tlsManagerSettings
          onlineLoop mgr cachePath users opts' aFlow lastRow


onlineLoop :: Manager ->  Path Abs File -> UserMap -> Opts -> ParamName Flow -> Maybe Row -> IO ()
onlineLoop mgr cachePath users opts flow = go
  where
    go lastRow = do
        req <- untag <$> messagesRequest (optsOrganisation opts) flow (baseMessageOptions $ rowMessageId <$> lastRow)
        let req' = authenticateRequest (optsToken opts) req
        res <- httpLbs req' mgr
        rows <- mapMaybe messageToRow <$> decode (responseBody res) :: IO [Row]
        saveRows cachePath rows
        lastRow' <- grepRow users opts rows
        -- Loop only if we got something
        when (isJust lastRow') $ go lastRow'

main :: IO ()
main = do
    settingsDirectory <- lookupSettingsDirectory
    token <- readAuthToken settingsDirectory
    let opts = info (helper <*> optsParser token) (fullDesc <> progDesc "Try --help if unsure" <> header "flowdock-grep - grep flowdock logs")
    parsedOpts <- execParser opts
    main' settingsDirectory (isNothing token) parsedOpts

onIOError :: a -> IOError -> IO a
onIOError x _ = return x

withIOError :: (IOError -> a) -> IOError -> a
withIOError = id
