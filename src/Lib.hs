{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators   #-}
module Lib
        ( AppConfiguration(..)
        , app
        )
where

import           Control.Exception.Base
import           Data.Aeson
import           Data.Aeson.TH
import           Data.ByteString
import           Data.Pool
import           Data.Time.Clock
import           Database.PostgreSQL.Simple
import           Servant

data User = User
  { userId        :: Int
  , userFirstName :: String
  , userLastName  :: String
  } deriving (Eq, Show)

$(deriveJSON defaultOptions ''User)

type API = "users" :> Get '[JSON] [User]

data AppConfiguration = AppConfiguration
  { databaseConnection :: ByteString
  , databasePoolMaxConns :: Int
  , databasePoolStripes :: Int
  , databasePoolTimeout :: NominalDiffTime
  , secretKey :: ByteString
  }

app :: AppConfiguration -> IO Application
app cfg = bracket setupDatabasePool destroyAllResources
        $ \pool -> return $ serve api $ server pool
    where
        setupDatabasePool = createPool
                (connectPostgreSQL $ databaseConnection cfg)
                close
                (databasePoolStripes cfg)
                (databasePoolTimeout cfg)
                (databasePoolMaxConns cfg)

api :: Proxy API
api = Proxy

server :: Pool Connection -> Server API
server _ = return users

users :: [User]
users = [User 1 "Isaac" "Newton", User 2 "Albert" "Einstein"]
