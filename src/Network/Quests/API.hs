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
import           Network.Quests.API.Reports
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

type InplaceApi a =
  Get '[JSON] [Short a] :<|>
  Capture "slug" T.Text :> (
    Get '[JSON] a :<|>
    ReqBody '[JSON] (Create a) :> Put '[JSON] a :<|>
    DeleteNoContent '[JSON] NoContent
  )

type ApiDocumentation = Get '[PlainText] T.Text

type CreateReportApi = ReqBody '[JSON] CreateReport :> Post '[JSON] NoContent

type BookshelvesApi = HierarchicalApi Bookshelf "id" Int32 (
    "report" :> CreateReportApi :<|>
    "roles" :> InplaceApi BookshelfRole
  )

type ChatsApi = HierarchicalApi Chat "id" Int32 (
    "messages" :> HierarchicalApi Message "id" Int32 (
        "report" :> CreateReportApi
      ) :<|>
    "report" :> CreateReportApi :<|>
    "roles" :> InplaceApi ChatRole
  )

type PollsApi = HierarchicalApi Poll "id" Int32 (
    "choices" :> HierarchicalApi Choice "id" Int32 (
        "report" :> CreateReportApi
      ) :<|>
    "report" :> CreateReportApi
  )

type QuestsApi = HierarchicalApi Quest "id" Int32 (
    "chapters" :> HierarchicalApi Chapter "index" Int32 (
        "passages" :> HierarchicalApi Passage "index" Int32 (
            "report" :> CreateReportApi
          ) :<|>
        "report" :> CreateReportApi
      ) :<|>
    "report" :> CreateReportApi :<|>
    "roles" :> InplaceApi QuestRole
  )

type ReportsApi = HierarchicalApi Report "id" Int32 EmptyAPI

type TagsApi = HierarchicalApi Tag "tag" T.Text (
    "report" :> CreateReportApi
  )

type UsersApi = HierarchicalApi User "slug" T.Text (
    "bans" :> HierarchicalApi Ban "id" Int32 EmptyAPI :<|>
    "session" :> HierarchicalApi Session "id" Int32 EmptyAPI :<|>
    "report" :> CreateReportApi
  )

type ApiVersion1 =
  "bookshelves" :> BookshelvesApi :<|>
  "chats" :> ChatsApi :<|>
  "polls" :> PollsApi :<|>
  "quests" :> QuestsApi :<|>
  "reports" :> ReportsApi :<|>
  "tags" :> TagsApi :<|>
  "users" :> UsersApi :<|>
  "ws" :> EmptyAPI

type ApiRoot = ApiDocumentation :<|> "v1" :> ApiVersion1

api :: Proxy ApiRoot
api = Proxy

apiDocs :: T.Text
apiDocs = T.pack . markdown $ docs api
