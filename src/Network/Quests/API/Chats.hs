{-# LANGUAGE TemplateHaskell #-}
module Network.Quests.API.Chats where

import           Data.Aeson.TH
import           Network.Quests.API.Common
import           Servant.Docs

data Chat = Chat {}

instance RestApi Chat where

instance ToSample Chat where
        toSamples _ = noSamples

$(deriveJSON defaultOptions ''Chat)
