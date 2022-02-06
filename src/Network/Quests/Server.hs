{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
module Network.Quests.Server
  ( apiV1Server
  )
where

import           Network.Quests.API
import           Network.Quests.GlobalContext
import           Servant

apiV1Server :: GlobalContext -> Server ApiVersion1
apiV1Server _ = errorWithoutStackTrace "unimplemented"
