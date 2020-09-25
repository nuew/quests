{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
module Network.Quests.API.Users
  ( Session
  , User (..)
  , CreateUser (..)
  , ShortUser (..)
  )
where

import           Data.Aeson.TH
import qualified Data.Text                     as T
import           Data.Time
import           Network.Quests.API.Common
import           Network.Quests.API.JSON
import           Network.URI
import           Servant.Docs

data UserSlug = UserSlug { userSlugSlug :: T.Text
                         , userSlugCreatedAt :: UTCTime
                         }

data User = User { userName :: T.Text
                 , userAvatar :: Maybe URI
                 , userRole :: Maybe UserRoleEnum
                 , userSlugs :: [UserSlug]
                 , userCreatedAt :: UTCTime
                 , userLastActive :: UTCTime
                 , userBiography :: T.Text
                 , userLocation :: T.Text
                 , userPronouns :: T.Text
                 , userWebsite :: T.Text
                 , userFollowers :: [Short User]
                 , userFollowing :: [Short User]
                 -- TODO bookshelves/quests, somehow
                 }

data ShortUser = ShortUser { shortUserName :: T.Text
                           , shortUserRole :: Maybe UserRoleEnum
                           , shortUserAvatar :: Maybe URI
                           , shortUserLastActive :: UTCTime
                           , shortUserUri :: URI
                           }

data CreateUser = CreateUserPassword { createUserName :: T.Text
                                     , createUserEmail :: T.Text
                                     , createUserPassword :: T.Text
                                     }
                | CreateUserOAuth


data UpdateUser = UpdateUser { updateUserName :: T.Text
                             , updateUserEmail :: T.Text
                             , updateUserAvatar :: Maybe URI
                             , updateUserBiography :: T.Text
                             , updateUserLocation :: T.Text
                             , updateUserPronouns :: T.Text
                             , updateUserWebsite :: T.Text
                             }

data UserRoleEnum = Administrator
                  | Moderator

data UserRole = UserRole { userRoleRole :: UserRoleEnum
                         , userRoleAppliedAt :: UTCTime
                         , userRoleAppliedBy :: Maybe (Ref User)
                         }

data Session = Session { sessionIpAddr :: T.Text -- should be good enough
                       , sessionUserAgent :: T.Text
                       , sessionCreatedAt :: UTCTime
                       , sessionLastActive :: UTCTime
                       , sessionExpiresAt :: Maybe UTCTime
                       }

instance RestApi User where
  type Short User = ShortUser
  type Create User = CreateUser
  type Update User = UpdateUser
  type CaptureName User = "slug"
  type CaptureType User = T.Text

instance RestApi UserRole

instance RestApi Session

instance ToSample User where
  toSamples _ = noSamples

instance ToSample ShortUser where
  toSamples _ = noSamples

instance ToSample CreateUser where
  toSamples _ = noSamples

instance ToSample UpdateUser where
  toSamples _ = noSamples

instance ToSample UserRole where
  toSamples _ = noSamples

instance ToSample Session where
  toSamples _ = noSamples

$(deriveJSON (jsonOptions "createUser") ''CreateUser)
$(deriveJSON (jsonOptions "session") ''Session)
$(deriveJSON (jsonOptions "shortUser") ''ShortUser)
$(deriveJSON (jsonOptions "updateUser") ''UpdateUser)
$(deriveJSON (jsonOptions "user") ''User)
$(deriveJSON (jsonOptions "userRole") ''UserRole)
$(deriveJSON (jsonOptions "userRole") ''UserRoleEnum)
$(deriveJSON (jsonOptions "userSlug") ''UserSlug)
