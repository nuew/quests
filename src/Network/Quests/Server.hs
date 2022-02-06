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
import           Network.Quests.API
import           Servant

apiV1Server :: Pool () -> IORef ChaChaDRG -> Server ApiVersion1
apiV1Server _ = errorWithoutStackTrace "unimplemented"
