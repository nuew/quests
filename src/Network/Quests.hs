{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Network.Quests
  ( AppConfiguration(..)
  , app
  , applyMigrations
  , dumpLayout
  )
where

import           Control.Exception.Base
import           Crypto.Random
import qualified Data.ByteString               as B
import           Data.IORef
import           Data.Pool
import qualified Data.Text                     as T
import           Data.Time.Clock
import           Network.Quests.API
import           Network.Quests.Server
import           Servant
import           Servant.Auth.Server

data AppConfiguration = AppConfiguration
  { databaseConnection :: B.ByteString
  , databasePoolMaxConns :: Int
  , databasePoolStripes :: Int
  , databasePoolTimeout :: NominalDiffTime
  , secretKey :: B.ByteString
  }

setupDatabasePool :: AppConfiguration -> IO (Pool ())
setupDatabasePool cfg = do
  pool <- createPool (return ())
                     (return $ return ())
                     (databasePoolStripes cfg)
                     (databasePoolTimeout cfg)
                     (databasePoolMaxConns cfg)
  return pool

server :: Pool () -> IORef ChaChaDRG -> Server ApiRoot
server pool rng = return apiDocs :<|> apiV1Server pool rng

apiContext :: AppConfiguration -> Context '[JWTSettings, CookieSettings]
apiContext cfg = jwtCtx :. defaultCookieSettings :. EmptyContext
  where jwtCtx = defaultJWTSettings . fromSecret $ secretKey cfg

app :: AppConfiguration -> IO Application
app cfg = bracket (setupDatabasePool cfg) destroyAllResources
  $ \pool -> serveWithContext api (apiContext cfg) . server pool <$> setupRng
  where setupRng = drgNew >>= newIORef

applyMigrations :: AppConfiguration -> IO ()
applyMigrations = return $ return ()

dumpLayout :: AppConfiguration -> String
dumpLayout = T.unpack . layoutWithContext api . apiContext
