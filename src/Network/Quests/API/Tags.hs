{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
module Network.Quests.API.Tags
  ( Tag
  )
where

import           Data.Aeson.TH
import qualified Data.Text                     as T
import           Network.Quests.API.Common
import           Network.Quests.API.JSON
import           Servant.Docs

newtype Tag = Tag T.Text

instance RestApi Tag where
  type CaptureName Tag = "tag"
  type CaptureType Tag = T.Text

instance ToSample Tag where
  toSamples _ = samples [Tag "nsfw", Tag "art quest"]

$(deriveJSON (jsonOptions "tag") ''Tag)
