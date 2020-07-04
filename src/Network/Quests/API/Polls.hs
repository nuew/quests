{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
module Network.Quests.API.Polls
        ( Poll
        , Choice
        )
where

import           Data.Aeson.TH
import qualified Data.Text                     as T
import           Network.Quests.API.JSON
import           Network.Quests.API.Common
import           Servant.Docs

data Poll = Poll

data Choice = Choice

instance RestApi Poll
instance RestApi Choice

instance ToSample Poll where
    toSamples _ = noSamples

instance ToSample Choice where
    toSamples _ = noSamples

$(deriveJSON (jsonOptions "choice") ''Choice)
$(deriveJSON (jsonOptions "poll") ''Poll)
