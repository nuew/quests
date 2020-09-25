{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
module Network.Quests.API.Reports
  ( Report
  , CreateReport
  )
where

import           Data.Aeson.TH
import qualified Data.Text                     as T
import           Data.Time
import           Network.Quests.API.Bookshelves
import           Network.Quests.API.Chats
import           Network.Quests.API.Common
import           Network.Quests.API.JSON
import           Network.Quests.API.Polls
import           Network.Quests.API.Quests
import           Network.Quests.API.Tags
import           Network.Quests.API.Users
import           Servant.Docs

data ReportSubject = ReportSubjectUser (Short User)
                   | ReportSubjectBookshelf (Short Bookshelf)
                   | ReportSubjectQuest (Short Quest)
                   | ReportSubjectChapter (Short Chapter)
                   | ReportSubjectPassage (Short Passage)
                   | ReportSubjectPoll (Short Poll)
                   | ReportSubjectChoice (Short Choice)
                   | ReportSubjectChat (Short Chat)
                   | ReportSubjectMessage (Short Message)
                   | ReportSubjectTag (Short Tag)

data ReportResolution = ReportResolution { reportResolutionAt :: UTCTime
                                         , reportResolutionBy :: Maybe (Short User)
                                         }

data Report = Report { reportCreatedAt :: UTCTime
                     , reportCreatedBy :: Maybe (Short User)
                     , reportResolved :: Maybe ReportResolution
                     , reportReason :: T.Text
                     , reportSubject :: ReportSubject
                     }

newtype CreateReport = CreateReport { createReportReason :: T.Text
                                    }

instance RestApi Report where
  type Create Report = CreateReport

instance ToSample Report where
  toSamples _ = noSamples

instance ToSample CreateReport where
  toSamples _ = noSamples

$(deriveJSON (jsonOptions "createReport") ''CreateReport)
$(deriveJSON (jsonOptions "report") ''Report)
$(deriveJSON (jsonOptions "reportResolution") ''ReportResolution)
$(deriveJSON (jsonOptions "reportSubject") ''ReportSubject)
