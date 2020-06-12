{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
module API where

import           Servant
import           Servant.Docs

type APIDocumentation = Get '[PlainText] String
type BookshelvesAPI = GetNoContent '[PlainText] NoContent
type ChatsAPI = GetNoContent '[PlainText] NoContent
type QuestsAPI = GetNoContent '[PlainText] NoContent
type TagsAPI = GetNoContent '[PlainText] NoContent
type UsersAPI = GetNoContent '[PlainText] NoContent
type WebsocketAPI = GetNoContent '[PlainText] NoContent

type AppAPI = APIDocumentation :<|>
    "bookshelves" :> BookshelvesAPI :<|>
    "chats" :> ChatsAPI :<|>
    "quests" :> QuestsAPI :<|>
    "tags" :> TagsAPI :<|>
    "users" :> UsersAPI :<|>
    "ws" :> WebsocketAPI

api :: Proxy AppAPI
api = Proxy

apiDocs :: String
apiDocs = markdown $ docs api

instance ToSample Char where
    toSamples _ = samples $ enumFrom '!'
