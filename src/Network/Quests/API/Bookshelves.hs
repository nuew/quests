{-# LANGUAGE TemplateHaskell #-}
module Network.Quests.API.Bookshelves where

import           Data.Aeson.TH
import           Network.Quests.API.Common
import           Servant.Docs

data Bookshelf = Bookshelf {}

instance RestApi Bookshelf where

instance ToSample Bookshelf where
        toSamples _ = noSamples

$(deriveJSON (jsonOptions "bookshelf") ''Bookshelf)
