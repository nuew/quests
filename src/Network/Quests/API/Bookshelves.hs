{-# LANGUAGE TemplateHaskell #-}
module Network.Quests.API.Bookshelves
        ( Bookshelf
        )
where

import           Data.Aeson.TH
import qualified Data.Text                     as T
import           Network.Quests.API.Common
import           Network.Quests.API.JSON
import           Network.Quests.API.Quests
import           Servant.Docs

data Bookshelf = Bookshelf { bookshelfName :: T.Text
                           , bookshelfDescription :: T.Text
                           , bookshelfIcon :: Char
                           , bookshelfEmailUpdates :: Bool
                           , bookshelfVisibility :: Visibility
                           , bookshelfQuests :: [Short Quest]
                           }

instance RestApi Bookshelf where

instance ToSample Bookshelf where
        toSamples _ = noSamples

$(deriveJSON (jsonOptions "bookshelf") ''Bookshelf)
