module Main where

import           Configuration.Dotenv
import qualified Data.ByteString               as B
import           Data.String
import qualified Lib                           as L
import           Network.Socket
import           Network.Wai
import           Network.Wai.Handler.Warp
import           System.Environment
import           Text.Read

data Configuration = Configuration
  { databaseConnection :: B.ByteString
  , secretKey :: Maybe B.ByteString
  , socketBindTo :: Maybe String
  , warpSettings :: Settings
  }

defaultConfiguration :: Configuration
defaultConfiguration = Configuration { databaseConnection = B.empty
                                     , secretKey          = Nothing
                                     , socketBindTo       = Nothing
                                     , warpSettings       = defaultSettings
                                     }

data ConfigurationHelper =
  DatabaseConnection | SecretKey | Socket |
  Warp (String -> Maybe (Settings -> Settings))

loadConfiguration :: IO Configuration
loadConfiguration = cfgPopulate defaultConfiguration <$> getSettings
    where
        -- Warp Settings Helpers
        toMaybeChw = return . Warp
        readWarpCh f = toMaybeChw $ fmap f . readMaybe
        warpApply cfg k v f = case f v of
                Just c  -> cfg { warpSettings = c $ warpSettings cfg }
                Nothing -> error $ "bad configuration: couldn't parse $" ++ k
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
        helperForVar "SECRET_KEY"        = Just SecretKey
        helperForVar "SOCKET"            = Just Socket
        helperForVar _                   = Nothing

        -- Go through all environment variables to find & apply relevant ones
        chApply cfg k v hf = case hf of
                DatabaseConnection -> cfg { databaseConnection = fromString v }
                SecretKey          -> cfg { secretKey = Just $ fromString v }
                Socket             -> cfg { socketBindTo = Just v }
                Warp f             -> warpApply cfg k v f
        cfgPopulate cfg ((k, v) : xs) =
                cfgPopulate (maybe cfg (chApply cfg k v) $ helperForVar k) xs
        cfgPopulate cfg [] = cfg

        -- Load environment variables from dotenv file
        maybeLoadDotEnv = onMissingFile (loadFile defaultConfig) $ return []
        getSettings     = maybeLoadDotEnv >> getEnvironment

main :: IO ()
main = do
        cfg <- loadConfiguration
        app <- L.app $ appCfgOfCfg cfg
        getWarp cfg >>= flip ($) app
    where
        unixSocket path = do
                sock <- socket AF_UNIX Stream defaultProtocol
                bind sock $ SockAddrUnix path
                listen sock maxListenQueue
                return sock
        getWarp cfg = case socketBindTo cfg of
                Just path ->
                        unixSocket path
                                >>= \s -> return $ runSettingsSocket
                                            (warpSettings cfg)
                                            s
                Nothing -> return $ runSettings (warpSettings cfg)

        appCfgOfCfg cfg = L.AppConfiguration
                { L.databaseConnection = databaseConnection cfg
                , L.secretKey          = case secretKey cfg of
                        Just sk -> sk
                        Nothing -> error "SECRET_KEY must be specified!"
                }
