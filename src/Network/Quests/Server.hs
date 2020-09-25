{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
module Network.Quests.Server
  ( apiV1Server
  )
where

import           Data.Pool
import           Crypto.Random
import           Data.IORef
import qualified Database.PostgreSQL.Simple    as PQ
import           Network.Quests.API
import           Servant

apiV1Server :: Pool PQ.Connection -> IORef ChaChaDRG -> Server ApiVersion1
apiV1Server pool = errorWithoutStackTrace "unimplemented"
