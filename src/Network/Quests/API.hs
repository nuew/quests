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

type HierSearchApi a = Get '[JSON] [Short a]
type HierCreateEndpoint a = PostCreated '[JSON] (Headers '[Header "Location" URI] a)
type HierCreateApi a = ReqBody '[JSON] (Create a) :> HierCreateEndpoint a
type HierItemGetApi a = Get '[JSON] a
type HierItemUpdateApi a = ReqBody '[JSON] (Update a) :> Put '[JSON] a
type HierItemDeleteApi a = DeleteNoContent '[JSON] NoContent
type HierItemApi a b c d = Capture b c :> (
    HierItemGetApi a :<|>
    HierItemUpdateApi a :<|>
    HierItemDeleteApi a :<|>
    d
  )
type HierarchicalApi a b c d =
  HierSearchApi a :<|>
  HierCreateApi a :<|>
  HierItemApi a b c d

type InplaceListApi a = Get '[JSON] [Short a]
type InplaceValueApi a = Get '[JSON] a
type InplaceSetApi a = ReqBody '[JSON] (Create a) :> Put '[JSON] a
type InplaceResetApi a = DeleteNoContent '[JSON] NoContent
type InplaceItemApi a = Capture "slug" T.Text :> (
    InplaceValueApi a :<|>
    InplaceSetApi a :<|>
    InplaceResetApi a
  )
type InplaceApi a = InplaceListApi a :<|> InplaceItemApi a

type ApiDocumentation = Get '[PlainText] T.Text

type CreateReportApi = ReqBody '[JSON] CreateReport :> Post '[JSON] NoContent

type BookshelfRolesApi = InplaceApi BookshelfRole
type BookshelvesApi = HierarchicalApi Bookshelf "id" Int32 (
    "report" :> CreateReportApi :<|>
    "roles" :> BookshelfRolesApi
  )

type MessagesApi = HierarchicalApi Message "id" Int32 (
    "report" :> CreateReportApi
  )
type ChatRolesApi = InplaceApi ChatRole
type ChatsApi = HierarchicalApi Chat "id" Int32 (
    "messages" :> MessagesApi :<|>
    "report" :> CreateReportApi :<|>
    "roles" :> ChatRolesApi
  )

type ChoicesApi = HierarchicalApi Choice "id" Int32 (
    "report" :> CreateReportApi
  )
type PollsApi = HierarchicalApi Poll "id" Int32 (
    "choices" :> ChoicesApi :<|>
    "report" :> CreateReportApi
  )

type PassagesApi = HierarchicalApi Passage "index" Int32 (
    "report" :> CreateReportApi
  )
type ChaptersApi = HierarchicalApi Chapter "index" Int32 (
    "passages" :> PassagesApi :<|>
    "report" :> CreateReportApi
  )
type QuestRolesApi = InplaceApi QuestRole
type QuestsApi = HierarchicalApi Quest "id" Int32 (
    "chapters" :> ChaptersApi :<|>
    "report" :> CreateReportApi :<|>
    "roles" :> QuestRolesApi
  )

type ReportsApi =
    HierSearchApi Report :<|>
    Capture "id" Int32 :> (HierItemGetApi Report :<|> HierItemDeleteApi Report)

type TagsApi = HierarchicalApi Tag "tag" T.Text (
    "report" :> CreateReportApi
  )

type BansApi = HierarchicalApi Ban "id" Int32 EmptyAPI
type SessionsApi = HierarchicalApi Session "id" Int32 EmptyAPI
type UsersApi = HierarchicalApi User "slug" T.Text (
    "bans" :> BansApi :<|>
    "sessions" :> SessionsApi :<|>
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
