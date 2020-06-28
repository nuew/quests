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
import           Network.Quests.API.Quests
import           Network.Quests.API.Tags
import           Network.Quests.API.Users
import           Network.URI
import           Servant.Docs

data ChatAssociation = ChatAssociationPrivateMessage [Short User]
                     | ChatAssociaionQuest (Short Quest)
                     | ChatAssociationTag (Short Tag)
                     | ChatAssociationUser (Short User)

data Chat = Chat { chatTopic :: T.Text
                 , chatAssociation :: ChatAssociation
                 , chatModerators :: [Ref User]
                 }

instance RestApi Chat where

instance ToSample Chat where
  toSamples _ = singleSample $ Chat
    "Discussion"
    (ChatAssociationTag $ URI "" Nothing "/tags/nsfw" "" "")
    []

$(deriveJSON (jsonOptions "chatAssociation") ''ChatAssociation)
$(deriveJSON (jsonOptions "chat") ''Chat)
