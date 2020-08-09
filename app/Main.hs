module Main where

import           Configuration.Dotenv
import           Control.Exception.Base
import           Control.Monad
import           Data.Aeson                     ( toJSON )
import           Data.Bifunctor
import qualified Data.ByteString               as B
import           Data.Char
import           Data.HashMap.Strict
import           Data.Maybe
import           Data.String
import           Data.Time.Clock
import           Data.Time.Format
import           Data.Version
import qualified Network.Quests                as Q
import qualified Network.Socket                as S
import           Network.Wai
import           Network.Wai.Handler.Warp
import           Paths_quests
import           System.Environment
import           System.IO
import           System.Log.Raven
import           System.Log.Raven.Transport.HttpConduit
import           System.Log.Raven.Types
import           Text.Read                      ( readMaybe )

data Configuration = Configuration
  { databaseConnection :: B.ByteString
  , databasePoolMaxConns :: Int
  , databasePoolStripes :: Int
  , databasePoolTimeout :: NominalDiffTime
  , secretKey :: Maybe B.ByteString
  , sentryService :: IO SentryService
  , socketBindTo :: Maybe String
  , warpSettings :: Settings
  }

cfgApply
        :: String
        -> String
        -> (String -> Maybe a)
        -> (a -> Configuration)
        -> Configuration
cfgApply k v pf apf = case pf v of
        Just a  -> apf a
        Nothing -> errorWithoutStackTrace cfgErrMsg
        where cfgErrMsg = "bad configuration: couldn't parse $" ++ k

defaultConfiguration :: Configuration
defaultConfiguration = Configuration { databaseConnection   = B.empty
                                     , databasePoolMaxConns = 16
                                     , databasePoolStripes  = 2
                                     , databasePoolTimeout  = 10
                                     , secretKey            = Nothing
                                     , sentryService        = disabledRaven
                                     , socketBindTo         = Nothing
                                     , warpSettings         = defaultSettings
                                     }

data ConfigurationHelper =
  DatabaseConnection | DatabaseMaxConns | DatabaseStripes | DatabaseTimeout |
  SecretKey | Sentry | Socket | Warp (String -> Maybe (Settings -> Settings))

loadConfiguration :: IO Configuration
loadConfiguration = cfgPopulate defaultConfiguration <$> getSettings
    where
        -- Warp Settings Helpers
        toMaybeChw = return . Warp
        readWarpCh f = toMaybeChw $ fmap f . readMaybe
        stringWarpCh f = toMaybeChw $ fmap f . return . fromString

        -- Configuration environment variables to helper function
        helperForVar "PORT"              = readWarpCh setPort
        helperForVar "HOST"              = stringWarpCh setHost
        helperForVar "TIMEOUT"           = readWarpCh setTimeout
        helperForVar "FD_CACHE_LENGTH"   = readWarpCh setFdCacheDuration
        helperForVar "STAT_CACHE_LENGTH" = readWarpCh setFileInfoCacheDuration
        helperForVar "NO_PARSE_PATH"     = readWarpCh setNoParsePath
        helperForVar "SERVER_NAME"       = stringWarpCh setServerName
        helperForVar "MAX_BODY_FLUSH"    = readWarpCh setMaximumBodyFlush
        helperForVar "SLOWLORIS_SIZE"    = readWarpCh setSlowlorisSize
        helperForVar "SHUTDOWN_TIMEOUT"  = readWarpCh setGracefulShutdownTimeout
        helperForVar "DB_CONN"           = Just DatabaseConnection
        helperForVar "DB_MAX_CONNS"      = Just DatabaseMaxConns
        helperForVar "DB_STRIPES"        = Just DatabaseStripes
        helperForVar "DB_TIMEOUT"        = Just DatabaseTimeout
        helperForVar "SECRET_KEY"        = Just SecretKey
        helperForVar "SENTRY_DSN"        = Just Sentry
        helperForVar "SOCKET"            = Just Socket
        helperForVar _                   = Nothing

        parseSecsDt   = parseTimeM True defaultTimeLocale "%s"
        questsRelease = "quests-" ++ showVersion version
        defaultRecord r = r { srRelease = Just questsRelease }
        sentryConn dsn = initRaven dsn defaultRecord sendRecord silentFallback

        -- Go through all environment variables to find & apply relevant ones
        caMutMaxConns cfg val = cfg { databasePoolMaxConns = val }
        caMutStripes cfg val = cfg { databasePoolStripes = val }
        caMutPoolTout cfg val = cfg { databasePoolTimeout = val }
        caMutWarp cfg val = cfg { warpSettings = val $ warpSettings cfg }

        chApply cfg k v hf = case hf of
                DatabaseConnection -> cfg { databaseConnection = fromString v }
                DatabaseMaxConns   -> cfgApply k v readMaybe $ caMutMaxConns cfg
                DatabaseStripes    -> cfgApply k v readMaybe $ caMutStripes cfg
                DatabaseTimeout -> cfgApply k v parseSecsDt $ caMutPoolTout cfg
                SecretKey          -> cfg { secretKey = Just $ fromString v }
                Sentry             -> cfg { sentryService = sentryConn v }
                Socket             -> cfg { socketBindTo = Just v }
                Warp f             -> cfgApply k v f $ caMutWarp cfg

        caCfgPop cfg (k, v) = maybe cfg (chApply cfg k v) $ helperForVar k
        cfgPopulate cfg (x : xs) = cfgPopulate (caCfgPop cfg x) xs
        cfgPopulate cfg []       = cfg

        -- Load environment variables from dotenv file
        maybeLoadDotEnv = onMissingFile (loadFile defaultConfig) $ return []
        getSettings     = maybeLoadDotEnv >> getEnvironment


-- Create Sentry Event metadata
sentryUpdateRecord :: Maybe Request -> SentryRecord -> SentryRecord
sentryUpdateRecord Nothing   r = r
sentryUpdateRecord (Just rq) r = r
        { srCulprit    = Just . show $ rawPathInfo rq
        , srServerName = show <$> requestHeaderHost rq
        , srTags       = union (srTags r) $ fromList
                                 [ ("http.method", show $ requestMethod rq)
                                 , ("http.version"     , show $ httpVersion rq)
                                 , ("http.query_string", show $ rawQueryString rq)
                                 , ("http.remote_host" , show $ remoteHost rq)
                                 ]
        , srExtra      = union (srExtra r) . fromList $ catMaybes
                [ requestHeaderRange rq >>= rhp "http.request.range"
                , requestHeaderReferer rq >>= rhp "http.referer"
                , requestHeaderUserAgent rq >>= rhp "http.user_agent"
                , case requestBodyLength rq of
                        KnownLength l ->
                                Just ("http.request.body_length", toJSON l)
                        ChunkedBody -> Nothing
                , Just ("http.is_secure", toJSON $ isSecure rq)
                , Just ("http.request.headers", toJSON headersMap)
                ]
        }
    where
        rhp name a = Just (name, showJSON a)
        showJSON   = toJSON . show
        headersMap = fromList . fmap (bimap show show) $ requestHeaders rq

-- Initialize Sentry Error Reporting Service
startSentry :: Configuration -> IO Configuration
startSentry cfg = mangleConfig <$> sentryService cfg
    where
        -- Create Event title
        fmtTitle Nothing e =
                "Exception before request could be parsed: " ++ show e
        fmtTitle (Just rq) e =
                "Exception while handling "
                        ++ show (requestMethod rq)
                        ++ " request for "
                        ++ show (rawPathInfo rq)
                        ++ ": "
                        ++ show e

        onException sentry rq e =
                when (defaultShouldDisplayException e)
                        $  hPutStrLn stderr (fmtTitle rq e)
                        >> register sentry
                                    "warp_except"
                                    Error
                                    (fmtTitle rq e)
                                    (sentryUpdateRecord rq)

        soeSentry s = setOnException (onException s) $ warpSettings cfg
        mangleConfig s = cfg { warpSettings = soeSentry s }

appCfgOfCfg cfg = Q.AppConfiguration
        { Q.databaseConnection   = databaseConnection cfg
        , Q.databasePoolMaxConns = databasePoolMaxConns cfg
        , Q.databasePoolStripes  = databasePoolStripes cfg
        , Q.databasePoolTimeout  = databasePoolTimeout cfg
        , Q.secretKey            = secretKeyCheck $ secretKey cfg
        }
    where
        secretKeyErrMsg = "SECRET_KEY must be specified!"
        secretKeyCheck sk = case sk of
                Just sk -> sk
                Nothing -> errorWithoutStackTrace secretKeyErrMsg

runServer :: Configuration -> IO ()
runServer cfg = do
        cfg' <- startSentry cfg
        app  <- Q.app $ appCfgOfCfg cfg'
        getWarp cfg app
    where
        unixSocket path = do
                sock <- S.socket S.AF_UNIX S.Stream S.defaultProtocol
                S.bind sock $ S.SockAddrUnix path
                S.listen sock S.maxListenQueue
                return sock
        getWarp cfg = case socketBindTo cfg of
                Just path -> \a -> bracket (unixSocket path) S.close
                        $ \s -> runSettingsSocket (warpSettings cfg) s a
                Nothing -> runSettings (warpSettings cfg)

main :: IO ()
main = do
        args <- fmap firstStrToLower getArgs
        cfg  <- loadConfiguration
        runModule args cfg
    where
        firstStrToLower []       = []
        firstStrToLower (x : xs) = fmap toLower x : xs

        runModule []              = runServer
        runModule (""        : _) = runModule []
        runModule ("migrate" : _) = Q.applyMigrations . appCfgOfCfg
        runModule _               = errorWithoutStackTrace "No such module."
