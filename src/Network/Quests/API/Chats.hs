{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Network.Quests.API.Chats
  ( Chat
  )
where

import           Data.Aeson.TH
import qualified Data.Text                     as T
import           Network.Quests.API.Common
import           Network.Quests.API.JSON
import           Network.Quests.API.Users
import           Servant.Docs

data Chat = Chat { chatTopic :: T.Text
                 , chatModerators :: [Ref User]
                 }

instance RestApi Chat where

instance ToSample Chat where
  toSamples _ = singleSample $ Chat
    "Discussion"
    []

$(deriveJSON (jsonOptions "chat") ''Chat)
