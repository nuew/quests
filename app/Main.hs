module Main where

import           Control.Exception.Base
import qualified Data.ByteString               as B
import           Data.Char
import           Data.String
import           Data.Time.Clock
import           Data.Time.Format
import           LoadEnv
import qualified Network.Quests                as Q
import qualified Network.Socket                as S
import           Network.Wai.Handler.Warp
import           System.Environment
import           Text.Read                      ( readMaybe )

data Configuration = Configuration
  { databaseConnection :: B.ByteString
  , databasePoolMaxConns :: Int
  , databasePoolStripes :: Int
  , databasePoolTimeout :: NominalDiffTime
  , secretKey :: Maybe B.ByteString
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
                                     , socketBindTo         = Nothing
                                     , warpSettings         = defaultSettings
                                     }

data ConfigurationHelper =
  DatabaseConnection | DatabaseMaxConns | DatabaseStripes | DatabaseTimeout |
  SecretKey | Socket | Warp (String -> Maybe (Settings -> Settings))

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
        helperForVar "SOCKET"            = Just Socket
        helperForVar _                   = Nothing

        parseSecsDt   = parseTimeM True defaultTimeLocale "%s"

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
                Socket             -> cfg { socketBindTo = Just v }
                Warp f             -> cfgApply k v f $ caMutWarp cfg

        caCfgPop cfg (k, v) = maybe cfg (chApply cfg k v) $ helperForVar k
        cfgPopulate cfg (x : xs) = cfgPopulate (caCfgPop cfg x) xs
        cfgPopulate cfg []       = cfg

        -- Load environment variables from dotenv file
        getSettings     = loadEnv >> getEnvironment

appCfgOfCfg :: Configuration -> Q.AppConfiguration
appCfgOfCfg cfg = Q.AppConfiguration
        { Q.databaseConnection   = databaseConnection cfg
        , Q.databasePoolMaxConns = databasePoolMaxConns cfg
        , Q.databasePoolStripes  = databasePoolStripes cfg
        , Q.databasePoolTimeout  = databasePoolTimeout cfg
        , Q.secretKey            = secretKeyCheck $ secretKey cfg
        }
    where
        secretKeyErrMsg = "SECRET_KEY must be specified!"
        secretKeyCheck msk = case msk of
                Just sk -> sk
                Nothing -> errorWithoutStackTrace secretKeyErrMsg

runServer :: Configuration -> IO ()
runServer cfg = do
        app <- Q.app $ appCfgOfCfg cfg
        getWarp cfg app
    where
        unixSocket path = do
                sock <- S.socket S.AF_UNIX S.Stream S.defaultProtocol
                S.bind sock $ S.SockAddrUnix path
                S.listen sock S.maxListenQueue
                return sock
        getWarp w_cfg = case socketBindTo w_cfg of
                Just path -> \a -> bracket (unixSocket path) S.close
                        $ \s -> runSettingsSocket (warpSettings w_cfg) s a
                Nothing -> runSettings (warpSettings w_cfg)

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
        runModule ("layout"  : _) = putStr . Q.dumpLayout . appCfgOfCfg
        runModule ("migrate" : _) = Q.applyMigrations . appCfgOfCfg
        runModule _               = errorWithoutStackTrace "No such module."
