{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
module Network.Quests.API.Users
        ( User
        )
where

import           Data.Aeson.TH
import qualified Data.Text                     as T
import           Data.Time
import           Network.Quests.API.Bookshelves
import           Network.Quests.API.Common
import           Network.Quests.API.JSON
import           Network.Quests.API.Quests
import           Network.URI
import           Servant.Docs

data User = User { userName :: T.Text
                 , userEmail :: T.Text
                 , userAvatar :: Maybe URI
                 , userCreated :: UTCTime
                 , userLastActive :: UTCTime
                 , userBiography :: T.Text
                 , userLocation :: T.Text
                 , userPronouns :: T.Text
                 , userWebsite :: T.Text
                 , userBookshelves :: [Short Bookshelf]
                 , userFollowers :: [Short User]
                 , userFollowing :: [Short User]
                 , userQuests :: [Short Quest]
                 }

data ShortUser = ShortUser { shortUserName :: T.Text
                           , shortUserAvatar :: Maybe URI
                           , shortUserLastActive :: UTCTime
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

instance RestApi User where
        type Short User = ShortUser
        type Create User = CreateUser
        type Update User = UpdateUser

uri1 = URI "" Nothing "/images/foo.png" "" ""
uri2 = URI "" Nothing "/images/baz.jpg" "" ""
time1 = read "1969-06-09 16:20:00Z" :: UTCTime
time2 = read "1970-01-01 00:00:00Z" :: UTCTime
time3 = read "2000-01-01 00:00:00-05:00" :: UTCTime
time4 = read "2020-06-16 16:27:00-04:00" :: UTCTime

instance ToSample User where
        toSamples _ = samples
                [ userSample "foo" "foo@example.com" (Just uri2) time1 time2
                , userSample "bar" "bar@example.net" Nothing     time3 time4
                ]
            where
                userSample name email avatar t1 t2 =
                        User name email avatar t1 t2 "" "" "" "" [] [] [] []

instance ToSample ShortUser where
        toSamples _ = samples
                [ ShortUser "foo" (Just uri1) time2
                , ShortUser "bar" Nothing     time4
                ]

instance ToSample CreateUser where
        toSamples _ = singleSample
                $ CreateUserPassword "foo" "foo@example.com" "Password1"

instance ToSample UpdateUser where
        toSamples _ = samples
                [ UpdateUser "foo" "foo@example.com" (Just uri1) "" "" "" ""
                , UpdateUser "bar" "bar@example.net" Nothing     "" "" "" ""
                ]

$(deriveJSON (jsonOptions "user") ''User)
$(deriveJSON (jsonOptions "shortUser") ''ShortUser)
$(deriveJSON (jsonOptions "createUser") ''CreateUser)
$(deriveJSON (jsonOptions "updateUser") ''UpdateUser)
