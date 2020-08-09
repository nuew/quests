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
type HierItemApi a b = Capture (CaptureName a) (CaptureType a) :> (
    HierItemGetApi a :<|>
    HierItemUpdateApi a :<|>
    HierItemDeleteApi a :<|>
    b
  )
type HierarchicalApi a b =
  HierSearchApi a :<|>
  HierCreateApi a :<|>
  HierItemApi a b

type InplaceListApi a = Get '[JSON] [Short a]
type InplaceValueApi a = Get '[JSON] a
type InplaceSetApi a = ReqBody '[JSON] (Create a) :> Put '[JSON] a
type InplaceResetApi a = DeleteNoContent '[JSON] NoContent
type InplaceItemApi a = Capture (CaptureName a) (CaptureType a) :> (
    InplaceValueApi a :<|>
    InplaceSetApi a :<|>
    InplaceResetApi a
  )
type InplaceApi a = InplaceListApi a :<|> InplaceItemApi a

type ApiDocumentation = Get '[PlainText] T.Text

type CreateReportApi = ReqBody '[JSON] CreateReport :> Post '[JSON] NoContent

type BookshelfRolesApi = InplaceApi BookshelfRole
type BookshelvesApi = HierarchicalApi Bookshelf (
    "report" :> CreateReportApi :<|>
    "roles" :> BookshelfRolesApi
  )

type MessagesApi = HierarchicalApi Message ("report" :> CreateReportApi)
type ChatRolesApi = InplaceApi ChatRole
type ChatsApi = HierarchicalApi Chat (
    "messages" :> MessagesApi :<|>
    "report" :> CreateReportApi :<|>
    "roles" :> ChatRolesApi
  )

type ChoicesApi = HierarchicalApi Choice ("report" :> CreateReportApi)
type PollsApi = HierarchicalApi Poll (
    "choices" :> ChoicesApi :<|>
    "report" :> CreateReportApi
  )

type PassagesApi = HierarchicalApi Passage ("report" :> CreateReportApi)
type ChaptersApi = HierarchicalApi Chapter (
    "passages" :> PassagesApi :<|>
    "report" :> CreateReportApi
  )
type QuestRolesApi = InplaceApi QuestRole
type QuestsApi = HierarchicalApi Quest (
    "chapters" :> ChaptersApi :<|>
    "report" :> CreateReportApi :<|>
    "roles" :> QuestRolesApi
  )

type ReportsApi = HierSearchApi Report :<|>
    Capture (CaptureName Report) (CaptureType Report) :>
      (HierItemGetApi Report :<|> HierItemDeleteApi Report)

type TagsApi = HierarchicalApi Tag ("report" :> CreateReportApi)

type BansApi = HierarchicalApi Ban EmptyAPI
type SessionsApi = HierarchicalApi Session EmptyAPI
type UsersApi = HierarchicalApi User (
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
