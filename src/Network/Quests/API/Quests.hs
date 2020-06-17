{-# LANGUAGE TemplateHaskell #-}
module Network.Quests.API.Quests where

import           Data.Aeson.TH
import           Network.Quests.API.Common
import           Servant.Docs

data Quest = Quest {}

instance RestApi Quest where

instance ToSample Quest where
        toSamples _ = noSamples

$(deriveJSON defaultOptions ''Quest)
