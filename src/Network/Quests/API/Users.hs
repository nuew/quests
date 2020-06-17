{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Network.Quests.API.Users where

import           Data.Aeson.TH
import qualified Data.Text                     as T
import           Data.Time
import           Network.Quests.API.Common
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
                 }

instance RestApi User where

instance ToSample User where
  toSamples _ = samples [ User "foo" "foo@example.com" (Just uri1) time1 time2 "" "" "" ""
                        , User "bar" "bar@example.net" Nothing time3 time4 "" "" "" ""
                        , User "baz" "baz@example.org" (Just uri2) time2 time4 "" "" "" ""
                        ]
    where
        uri1 = URI "" Nothing "/images/foo.png" "" ""
        uri2 = URI "" Nothing "/images/baz.jpg" "" ""
        time1 = read "1969-06-09 16:20:00Z" :: UTCTime
        time2 = read "1970-01-01 00:00:00Z" :: UTCTime
        time3 = read "2000-01-01 00:00:00-05:00" :: UTCTime
        time4 = read "2020-06-16 16:27:00-04:00" :: UTCTime

$(deriveJSON defaultOptions ''User)
