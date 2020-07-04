{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
module Network.Quests.API where

import           Data.Int
import qualified Data.Text                     as T
import           Network.Quests.API.Bans
import           Network.Quests.API.Bookshelves
import           Network.Quests.API.Chats
import           Network.Quests.API.Common
import           Network.Quests.API.Polls
import           Network.Quests.API.Quests
import           Network.Quests.API.Tags
import           Network.Quests.API.Users
import           Servant
import           Servant.Docs

type HierarchicalApi a b c d =
  Get '[JSON] [Short a] :<|>
  ReqBody '[JSON] (Create a) :>
    PostCreated '[JSON] (Headers '[Header "Location" URI] a) :<|>
  Capture b c :> (
    Get '[JSON] a :<|>
    ReqBody '[JSON] (Update a) :> Put '[JSON] a :<|>
    DeleteNoContent '[JSON] NoContent :<|>
    d
  )

type SimpleHierarchicalApi a = HierarchicalApi a "id" Int32 EmptyAPI

type InplaceApi a =
  Get '[JSON] [Short a] :<|>
  Capture "slug" T.Text :> (
    Get '[JSON] a :<|>
    ReqBody '[JSON] (Create a) :> Put '[JSON] a :<|>
    DeleteNoContent '[JSON] NoContent
  )

type ApiDocumentation = Get '[PlainText] T.Text

type BookshelvesApi = HierarchicalApi Bookshelf "id" Int32 (
    "roles" :> InplaceApi BookshelfRole
  )

type ChatsApi = HierarchicalApi Chat "id" Int32 (
    "messages" :> SimpleHierarchicalApi Message :<|>
    "roles" :> InplaceApi ChatRole
  )

type PollsApi = HierarchicalApi Poll "id" Int32 (
    "choices" :> SimpleHierarchicalApi Choice
  )

type QuestsApi = HierarchicalApi Quest "id" Int32 (
    "chapters" :> HierarchicalApi Chapter "index" Int32 (
        "passages" :> HierarchicalApi Passage "index" Int32 EmptyAPI
      ) :<|>
    "roles" :> InplaceApi QuestRole
  )

type TagsApi = SimpleHierarchicalApi Tag

type UsersApi = HierarchicalApi User "slug" T.Text (
    "session" :> SimpleHierarchicalApi Session :<|>
    "bans" :> SimpleHierarchicalApi Ban
  )

type ApiVersion1 =
  "bookshelves" :> BookshelvesApi :<|>
  "chats" :> ChatsApi :<|>
  "polls" :> PollsApi :<|>
  "quests" :> QuestsApi :<|>
  "tags" :> TagsApi :<|>
  "users" :> UsersApi :<|>
  "ws" :> EmptyAPI

type ApiRoot = ApiDocumentation :<|> "v1" :> ApiVersion1

api :: Proxy ApiRoot
api = Proxy

apiDocs :: T.Text
apiDocs = T.pack . markdown $ docs api
