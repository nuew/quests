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
import           Data.Time
import           Network.Quests.API.Common
import           Network.Quests.API.JSON
import           Network.Quests.API.Users
import           Network.URI
import           Servant.Docs

data Chat = Chat { chatTopic :: T.Text
                 , chatModerators :: [Ref User]
                 , chatOpen :: Bool
                 , chatVoicedOnly :: Bool
                 }

data ChatRoleEnum = ChatRoleOwner
                  | ChatRoleModerator
                  | ChatRoleVoiced

data ChatRole = ChatRole { chatRoleRole :: ChatRoleEnum
                         , chatRoleAppliedAt :: UTCTime
                         , chatRoleAppliedBy :: Maybe (Ref User)
                         }

data Message = Message { messageAuthor :: Short User
                       , messageText :: T.Text
                       , messageAttachment :: Maybe URI
                       , messagePostedAt :: UTCTime
                       , messageLastUpdatedAt :: UTCTime
                       }

instance RestApi Chat

instance RestApi ChatRole where
  type CaptureName ChatRole = "slug"
  type CaptureType ChatRole = T.Text

instance RestApi Message

instance ToSample Chat where
  toSamples _ = noSamples

instance ToSample ChatRole where
  toSamples _ = noSamples

instance ToSample Message where
  toSamples _ = noSamples

$(deriveJSON (jsonOptions "chat") ''Chat)
$(deriveJSON (jsonOptions "chatRole") ''ChatRole)
$(deriveJSON (jsonOptions "chatRole") ''ChatRoleEnum)
$(deriveJSON (jsonOptions "message") ''Message)
