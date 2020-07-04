{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
module Network.Quests.API.Bans
        ( Ban
        )
where

import           Data.Aeson.TH
import qualified Data.Text                     as T
import           Network.Quests.API.JSON
import           Network.Quests.API.Common
import           Servant.Docs

data Ban = Ban

instance RestApi Ban

instance ToSample Ban where
    toSamples _ = noSamples

$(deriveJSON (jsonOptions "ban") ''Ban)
