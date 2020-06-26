{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Network.Quests.API.Quests (Quest) where

import           Data.Aeson.TH
import qualified Data.Text                     as T
import           Network.Quests.API.Common
import           Network.Quests.API.JSON
import           Network.Quests.API.Tags
import           Network.URI
import           Servant.Docs

data Quest = Quest { questName :: T.Text
                   , questTeaser :: T.Text
                   , questBanner :: Maybe URI
                   , questVisibility :: Visibility
                   , questTags :: [Short Tag]
                   }

instance RestApi Quest where

instance ToSample Quest where
        toSamples _ = singleSample $ Quest "Sample Quest" "" Nothing Public []

$(deriveJSON (jsonOptions "quest") ''Quest)
