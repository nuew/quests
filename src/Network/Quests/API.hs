{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
module Network.Quests.API where

import qualified Data.Text                     as T
import           Network.Quests.API.Bookshelves
import           Network.Quests.API.Chats
import           Network.Quests.API.Common
import           Network.Quests.API.Quests
import           Network.Quests.API.Tags
import           Network.Quests.API.Users
import           Servant
import           Servant.Docs

type RestCollection a =
  Get '[JSON] [a] :<|>
  ReqBody '[JSON] (Create a) :> PostCreated '[JSON] (Headers '[Header "Location" URI] a)

type RestObject a =
  Get '[JSON] a :<|>
  ReqBody '[JSON] (Update a) :> Put '[JSON] a :<|>
  DeleteNoContent '[JSON] NoContent

type IdRestObject a =  Capture "id" Integer :> RestObject a
type SlugRestObject a = Capture "slug" T.Text :> RestObject a

type APIDocumentation = Get '[PlainText] T.Text
type BookshelvesAPI = RestCollection Bookshelf :<|> IdRestObject Bookshelf
type ChatsAPI = RestCollection Chat :<|> IdRestObject Chat
type QuestsAPI = RestCollection Quest :<|> SlugRestObject Quest
type TagsAPI = RestCollection Tag :<|> SlugRestObject Tag
type UsersAPI = RestCollection User :<|> SlugRestObject User
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

apiDocs :: T.Text
apiDocs = T.pack . markdown $ docs api
