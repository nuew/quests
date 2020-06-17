{-# LANGUAGE TemplateHaskell #-}
module Network.Quests.API.Tags where

import           Data.Aeson.TH
import           Network.Quests.API.Common
import           Servant.Docs

data Tag = Tag {}

instance RestApi Tag where

instance ToSample Tag where
        toSamples _ = noSamples

$(deriveJSON (jsonOptions "tag") ''Tag)
