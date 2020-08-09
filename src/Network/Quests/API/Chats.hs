{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
module Network.Quests.API.Chats
  ( Chat
  , ChatRole
  , Message
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

data ChatRole = ChatRole
data Message = Message

instance RestApi Chat

instance RestApi ChatRole where
  type CaptureName ChatRole = "slug"
  type CaptureType ChatRole = T.Text

instance RestApi Message

instance ToSample Chat where
  toSamples _ = singleSample $ Chat
    "Discussion"
    []

instance ToSample ChatRole where
  toSamples _ = noSamples

instance ToSample Message where
  toSamples _ = noSamples

$(deriveJSON (jsonOptions "chat") ''Chat)
$(deriveJSON (jsonOptions "chatRole") ''ChatRole)
$(deriveJSON (jsonOptions "message") ''Message)
