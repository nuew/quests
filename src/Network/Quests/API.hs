{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
module Network.Quests.API where

import           Data.Aeson.TH
import qualified Data.ByteString               as B
import qualified Data.Text                     as T
import           Data.Time
import           Network.URI
import           Servant
import           Servant.Docs

class RestApi a where
     type Short a :: *
     type Short a = a

     type Create a :: *
     type Create a = a

     type Update a :: *
     type Update a = a

type RestCollection a =
     Get '[JSON] [a] :<|>
     ReqBody '[JSON] (Create a) :> PostCreated '[JSON] (Headers '[Header "Location" URI] a)

type RestObject a =
     Get '[JSON] a :<|>
     ReqBody '[JSON] (Update a) :> Put '[JSON] a :<|>
     DeleteNoContent '[JSON] NoContent

type SimpleRestApi a = RestCollection a :<|> RestObject a

type IdRestApi a = Capture "id" Integer :> SimpleRestApi a
type SlugRestApi a = Capture "slug" T.Text :> SimpleRestApi a

instance ToHttpApiData URI where
  toUrlPiece uri = T.pack $ uriToString id uri ""

instance ToCapture (Capture "id" Integer) where
  toCapture _ =
    DocCapture "id" "The numeric identifier that corresponds to the resource."

instance ToCapture (Capture "slug" T.Text) where
  toCapture _ = DocCapture "slug" "The slug that corresponds to the resource."

instance ToSample URI where
  toSamples _ =
    singleSample $ URI "https:" (Just $ URIAuth "" "example.com" "") "/" "" ""

--

data Bookshelf = Bookshelf {}

instance RestApi Bookshelf where

instance ToSample Bookshelf where
  toSamples _ = noSamples

$(deriveJSON defaultOptions ''Bookshelf)

data Chat = Chat {}

instance RestApi Chat where

instance ToSample Chat where
  toSamples _ = noSamples

$(deriveJSON defaultOptions ''Chat)

data Quest = Quest {}

instance RestApi Quest where

instance ToSample Quest where
  toSamples _ = noSamples

$(deriveJSON defaultOptions ''Quest)

data Tag = Tag {}

instance RestApi Tag where

instance ToSample Tag where
  toSamples _ = noSamples

$(deriveJSON defaultOptions ''Tag)

data User = User { userName :: T.Text
                 , userEmail :: T.Text
                 --, userAvatar :: Maybe URI
                 , userCreated :: UTCTime
                 , userLastActive :: UTCTime
                 , userBiography :: T.Text
                 , userLocation :: T.Text
                 , userPronouns :: T.Text
                 , userWebsite :: T.Text
                 }

instance RestApi User where

instance ToSample User where
  toSamples _ = samples [ User "foo" "foo@example.com" time1 time2 "" "" "" ""
                        , User "bar" "bar@example.net" time3 time4 "" "" "" ""
                        , User "baz" "baz@example.org" time2 time4 "" "" "" ""
                        ]
    where
        time1 = read "1969-06-09 16:20:00Z" :: UTCTime
        time2 = read "1970-01-01 00:00:00Z" :: UTCTime
        time3 = read "2000-01-01 00:00:00-05:00" :: UTCTime
        time4 = read "2020-06-16 16:27:00-04:00" :: UTCTime

$(deriveJSON defaultOptions ''User)

--

type APIDocumentation = Get '[PlainText] T.Text
type BookshelvesAPI = IdRestApi Bookshelf
type ChatsAPI = IdRestApi Chat
type QuestsAPI = SlugRestApi Quest
type TagsAPI = SlugRestApi Tag
type UsersAPI = SlugRestApi User
type WebsocketAPI = GetNoContent '[PlainText] NoContent

type ApiVersion1 =
    "bookshelves" :> BookshelvesAPI :<|>
    "chats" :> ChatsAPI :<|>
    "quests" :> QuestsAPI :<|>
    "tags" :> TagsAPI :<|>
    "users" :> UsersAPI :<|>
    "ws" :> WebsocketAPI

type ApiRoot = APIDocumentation :<|> "v1" :> ApiVersion1

api :: Proxy ApiRoot
api = Proxy

--

apiDocs :: T.Text
apiDocs = T.pack . markdown $ docs api

instance ToSample T.Text where
  toSamples _ = singleSample $ "Lorem ipsum dolor sit amet"
