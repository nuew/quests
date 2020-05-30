{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators   #-}
module Lib
        ( AppConfiguration(..)
        , app
        )
where

import           Data.ByteString
import           Data.Aeson
import           Data.Aeson.TH
import           Data.Pool
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
  , secretKey :: ByteString
  }

app :: AppConfiguration -> IO Application
app cfg = setupDatabasePool >>= \pool -> return $ serve api $ server pool
    where
        setupDatabasePool = createPool
                (connectPostgreSQL $ databaseConnection cfg)
                close
                2
                60
                10

api :: Proxy API
api = Proxy

server :: Pool Connection -> Server API
server _ = return users

users :: [User]
users = [User 1 "Isaac" "Newton", User 2 "Albert" "Einstein"]
