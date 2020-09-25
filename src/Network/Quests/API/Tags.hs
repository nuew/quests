{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
module Network.Quests.API.Tags
  ( Tag
  , TagApplication
  )
where

import           Data.Aeson.TH
import qualified Data.Text                     as T
import           Data.Time
import           Network.Quests.API.Common
import           Network.Quests.API.JSON
import           Network.Quests.API.Users
import           Servant.Docs

newtype Tag = Tag T.Text

data TagApplication = TagApplication { tagApplicationTag :: Short Tag
                                     , tagApplicationAppliedAt :: UTCTime
                                     , tagApplicationAppliedBy :: Maybe (Ref User)
                                     }

instance RestApi Tag where
  type CaptureName Tag = "tag"
  type CaptureType Tag = T.Text

instance ToSample Tag where
  toSamples _ = noSamples

$(deriveJSON (jsonOptions "tag") ''Tag)
$(deriveJSON (jsonOptions "tagApplication") ''TagApplication)
