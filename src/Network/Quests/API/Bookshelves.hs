{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
module Network.Quests.API.Bookshelves
  ( Bookshelf
  , BookshelfRole
  )
where

import           Data.Aeson.TH
import qualified Data.Text                     as T
import           Data.Time
import           Network.Quests.API.Common
import           Network.Quests.API.JSON
import           Network.Quests.API.Quests
import           Network.Quests.API.Users
import           Servant.Docs

data Bookshelf = Bookshelf { bookshelfName :: T.Text
                           , bookshelfEditors :: [Ref User]
                           , bookshelfDescription :: T.Text
                           , bookshelfIcon :: Char
                           , bookshelfVisibility :: Visibility
                           , bookshelfCreatedAt :: UTCTime
                           , bookshelfQuests :: [Short Quest]
                           }
data BookshelfRoleEnum = BookshelfRoleOwner
                       | BookshelfRoleEditor
                       | BookshelfRoleVisitor

data BookshelfRole = BookshelfRole { bookshelfRoleRole :: BookshelfRoleEnum
                                   , bookshelfRoleEmailUpdates :: Bool
                                   , bookshelfRoleAppliedAt :: UTCTime
                                   , bookshelfRoleAppliedBy :: Maybe (Ref User)
                                   }

instance RestApi Bookshelf

instance RestApi BookshelfRole where
  type CaptureName BookshelfRole = "slug"
  type CaptureType BookshelfRole = T.Text

instance ToSample Bookshelf where
  toSamples _ = noSamples

instance ToSample BookshelfRole where
  toSamples _ = noSamples

$(deriveJSON (jsonOptions "bookshelf") ''Bookshelf)
$(deriveJSON (jsonOptions "bookshelfRole") ''BookshelfRole)
$(deriveJSON (jsonOptions "bookshelfRole") ''BookshelfRoleEnum)
