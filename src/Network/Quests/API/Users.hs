{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
module Network.Quests.API.Users where

import           Data.Aeson.TH
import qualified Data.Text                     as T
import           Data.Time
import           Network.Quests.API.Common
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
                 , userQuests :: [Short Quest]
                 }

data UserCreate = UserCreatePassword { userCreateName :: T.Text
                                     , userCreateEmail :: T.Text
                                     , userCreatePassword :: T.Text
                                     }
                | UserCreateOAuth


data UserUpdate = UserUpdate { userUpdateName :: T.Text
                             , userUpdateEmail :: T.Text
                             , userUpdateAvatar :: Maybe URI
                             , userUpdateBiography :: T.Text
                             , userUpdateLocation :: T.Text
                             , userUpdatePronouns :: T.Text
                             , userUpdateWebsite :: T.Text
                             }

instance RestApi User where
    type Create User = UserCreate
    type Update User = UserUpdate

instance ToSample User where
  toSamples _ = samples [ User "foo" "foo@example.com" (Just uri1) time1 time2 "" "" "" "" []
                        , User "bar" "bar@example.net" Nothing time3 time4 "" "" "" "" []
                        , User "baz" "baz@example.org" (Just uri2) time2 time4 "" "" "" "" []
                        ]
    where
        uri1 = URI "" Nothing "/images/foo.png" "" ""
        uri2 = URI "" Nothing "/images/baz.jpg" "" ""
        time1 = read "1969-06-09 16:20:00Z" :: UTCTime
        time2 = read "1970-01-01 00:00:00Z" :: UTCTime
        time3 = read "2000-01-01 00:00:00-05:00" :: UTCTime
        time4 = read "2020-06-16 16:27:00-04:00" :: UTCTime

instance ToSample UserCreate where
  toSamples _ = singleSample $ UserCreatePassword "foo" "foo@example.com" "Password1"

instance ToSample UserUpdate where
  toSamples _ = samples [ UserUpdate "foo" "foo@example.com" (Just uri1) "" "" "" ""
                        , UserUpdate "bar" "bar@example.net" Nothing "" "" "" ""
                        , UserUpdate "baz" "baz@example.org" (Just uri2) "" "" "" ""
                        ]
    where
        uri1 = URI "" Nothing "/images/foo.png" "" ""
        uri2 = URI "" Nothing "/images/baz.jpg" "" ""

$(deriveJSON (jsonOptions "user") ''User)
$(deriveJSON (jsonOptions "userCreate") ''UserCreate)
$(deriveJSON (jsonOptions "userUpdate") ''UserUpdate)
