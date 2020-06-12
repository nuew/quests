{-# LANGUAGE OverloadedStrings #-}
module Lib
        ( AppConfiguration(..)
        , app
        )
where

import           API
import           Control.Exception.Base
import           Control.Monad.IO.Class
import           Data.Aeson
import           Data.Aeson.TH
import qualified Data.ByteString               as B
import           Data.Pool
import           Data.Time.Clock
import qualified Database.PostgreSQL.Simple    as PG
import           Database.PostgreSQL.Simple.Migration
import           Servant
import           Servant.Auth.Server

data AppConfiguration = AppConfiguration
  { databaseConnection :: B.ByteString
  , databasePoolMaxConns :: Int
  , databasePoolStripes :: Int
  , databasePoolTimeout :: NominalDiffTime
  , secretKey :: B.ByteString
  }

app :: AppConfiguration -> IO Application
app cfg = bracket setupDatabasePool destroyAllResources
        $ \pool -> return (serveWithContext api ctx $ server pool)
    where
        jwtCtx = defaultJWTSettings $ fromSecret $ secretKey cfg
        ctx    = jwtCtx :. defaultCookieSettings :. EmptyContext

        migrateOrThrow f = case f of
                MigrationSuccess -> return ()
                MigrationError e -> error e
        migrationCmds =
                [MigrationInitialization, MigrationDirectory "./migrations/"]
        doMigrations conn =
                PG.withTransaction conn $ runMigrations False conn migrationCmds
        setupDatabasePool = do
                pool <- createPool
                        (PG.connectPostgreSQL $ databaseConnection cfg)
                        PG.close
                        (databasePoolStripes cfg)
                        (databasePoolTimeout cfg)
                        (databasePoolMaxConns cfg)
                withResource pool doMigrations >>= migrateOrThrow
                return pool

server :: Pool PG.Connection -> Server AppAPI
server pool =
        return apiDocs
                :<|> undefined
                :<|> undefined
                :<|> undefined
                :<|> undefined
                :<|> undefined
                :<|> undefined
