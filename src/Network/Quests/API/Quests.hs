{-# LANGUAGE TemplateHaskell #-}
module Network.Quests.API.Quests (Quest) where

import           Data.Aeson.TH
import           Network.Quests.API.Common
import           Network.Quests.API.JSON
import           Servant.Docs

data Quest = Quest {}

instance RestApi Quest where

instance ToSample Quest where
        toSamples _ = noSamples

$(deriveJSON (jsonOptions "quest") ''Quest)
