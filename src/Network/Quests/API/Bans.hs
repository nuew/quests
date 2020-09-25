{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
module Network.Quests.API.Bans
  ( Ban
  )
where

import           Data.Aeson.TH
import qualified Data.Text                     as T
import           Data.Time
import           Network.Quests.API.Common
import           Network.Quests.API.Chats
import           Network.Quests.API.JSON
import           Network.Quests.API.Quests
import           Network.Quests.API.Users
import           Servant.Docs

data BanScope = BanScopeGlobal
              | BanScopeQuest (Short Quest)
              | BanScopeChat (Short Chat)

data Ban = Ban { banUser :: Short User
               , banAppliedAt :: UTCTime
               , banAppliedBy :: Maybe (Ref User)
               , banExpiresAt :: Maybe UTCTime
               , banReason :: T.Text
               , banScope :: BanScope
               }

instance RestApi Ban

instance ToSample Ban where
  toSamples _ = noSamples

instance ToSample BanScope where
  toSamples _ = noSamples

$(deriveJSON (jsonOptions "ban") ''Ban)
$(deriveJSON (jsonOptions "banScope") ''BanScope)
