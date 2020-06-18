{-# LANGUAGE TemplateHaskell #-}
module Network.Quests.API.Chats (Chat) where

import           Data.Aeson.TH
import           Network.Quests.API.Common
import           Network.Quests.API.JSON
import           Servant.Docs

data Chat = Chat {}

instance RestApi Chat where

instance ToSample Chat where
        toSamples _ = noSamples

$(deriveJSON (jsonOptions "chat") ''Chat)
