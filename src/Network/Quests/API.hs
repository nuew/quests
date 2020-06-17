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

apiDocs :: T.Text
apiDocs = T.pack . markdown $ docs api
