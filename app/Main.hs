module Main where

import           Configuration.Dotenv
import           Data.ByteString
import           Data.String
import           Lib
import           Network.Socket
import           Network.Wai
import           Network.Wai.Handler.Warp
import           System.Environment
import           Text.Read

data Configuration = Configuration {
        cookieSecret :: Maybe ByteString,
        socketBindTo :: Maybe String,
        warpSettings :: Settings
    }

data ConfigurationHelper =
    Secret | Socket | Warp (String -> Maybe (Settings -> Settings))

appConfiguration :: IO Configuration
appConfiguration = flip cfgPopulate defaultConfiguration <$> getSettings
    where
        defaultConfiguration = Configuration { cookieSecret = Nothing
                                             , socketBindTo = Nothing
                                             , warpSettings = defaultSettings
                                             }

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
        helperForVar "SECRET"            = Just Secret
        helperForVar "SOCKET"            = Just Socket
        helperForVar "SHUTDOWN_TIMEOUT"  = readWarpCh setGracefulShutdownTimeout
        helperForVar _                   = Nothing

        -- Go through all environment variables to find & apply relevant ones
        chApply cfg k v hf = case hf of
                Secret -> cfg { cookieSecret = Just $ fromString v }
                Socket -> cfg { socketBindTo = Just v }
                Warp f -> warpApply cfg k v f
        cfgPopulate ((k, v) : xs) cfg =
                cfgPopulate xs $ maybe cfg (chApply cfg k v) $ helperForVar k
        cfgPopulate [] cfg = cfg

        -- Load environment variables from dotenv file
        maybeLoadDotEnv = onMissingFile (loadFile defaultConfig) $ return []
        getSettings     = maybeLoadDotEnv >> getEnvironment

main :: IO ()
main = appConfiguration >>= runWarp app
    where
        unixSocket path = do
                sock <- socket AF_UNIX Stream defaultProtocol
                bind sock $ SockAddrUnix path
                listen sock maxListenQueue
                return sock
        runWarp app cfg = case socketBindTo cfg of
                Just path -> unixSocket path
                        >>= \s -> runSettingsSocket (warpSettings cfg) s app
                Nothing -> runSettings (warpSettings cfg) app
