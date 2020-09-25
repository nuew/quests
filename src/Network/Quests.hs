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
import           Control.Monad.IO.Class
import           Data.Aeson
import           Data.Aeson.TH
import qualified Data.ByteString               as B
import           Data.FileEmbed
import           Data.Pool
import qualified Data.Text                     as T
import           Data.Time.Clock
import qualified Database.PostgreSQL.Simple    as PG
import           Database.PostgreSQL.Simple.Migration
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

migrations :: MigrationCommand
migrations = MigrationCommands $ MigrationInitialization : migrationScripts
    where
        migrationDirectory = $(embedDir "./migrations/")
        migrationScripts   = fmap (uncurry MigrationScript) migrationDirectory

doMigration :: MigrationCommand -> PG.Connection -> IO ()
doMigration migrations conn = PG.withTransaction conn $ do
        PG.execute_ conn "SET LOCAL client_min_messages = WARNING;"
        runMigration ctx >>= migrateOrFail
    where
        ctx = MigrationContext { migrationContextCommand    = migrations
                               , migrationContextVerbose    = False
                               , migrationContextConnection = conn
                               }
        migrateOrFail f = case f of
                MigrationSuccess -> return ()
                MigrationError e -> errorWithoutStackTrace $ migrateErrMsg e
        migrateErrMsg e = "Could not verify DB migrations (" ++ e ++ ")"

setupDatabasePool :: AppConfiguration -> IO (Pool PG.Connection)
setupDatabasePool cfg = do
        pool <- createPool
                (PG.connectPostgreSQL $ databaseConnection cfg)
                PG.close
                (databasePoolStripes cfg)
                (databasePoolTimeout cfg)
                (databasePoolMaxConns cfg)
        withResource pool . doMigration $ MigrationValidation migrations
        return pool

server :: Pool PG.Connection -> Server ApiRoot
server pool = return apiDocs :<|> apiV1Server pool

apiContext :: AppConfiguration -> Context '[JWTSettings, CookieSettings]
apiContext cfg = jwtCtx :. defaultCookieSettings :. EmptyContext
    where jwtCtx = defaultJWTSettings . fromSecret $ secretKey cfg

app :: AppConfiguration -> IO Application
app cfg = bracket (setupDatabasePool cfg) destroyAllResources
        $ \pool -> return $ serveWithContext api (apiContext cfg) (server pool)

applyMigrations :: AppConfiguration -> IO ()
applyMigrations cfg = conn >>= doMigration migrations
    where conn = PG.connectPostgreSQL $ databaseConnection cfg

dumpLayout :: AppConfiguration -> String
dumpLayout = T.unpack . layoutWithContext api . apiContext
