{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
module Network.Quests.API.Quests
  ( Chapter
  , Passage
  , Quest
  , QuestRole
  )
where

import           Data.Aeson.TH
import qualified Data.Text                     as T
import           Data.Time.Clock
import           Network.Quests.API.Common
import           Network.Quests.API.JSON
import           Network.Quests.API.Tags
import           Network.URI
import           Servant.Docs

data PassageContents = TextualPassage T.Text
                     | DummyPassage -- TODO removeme

data Passage = Passage { passageCreated :: UTCTime
                       , passageUpdated :: UTCTime
                       , passageContents :: PassageContents
                       }

data UpdatePassage = UpdatePassage { updatePassageContents :: PassageContents
                                   , updatePassageIndex :: Int
                                   }

data Chapter = Chapter { chapterName :: T.Text
                       , chapterIsAppendix :: Bool
                       , chapterPassages :: [Passage]
                       }

data ShortChapter = ShortChapter { shortChapterName :: T.Text
                                 , shortChapterIsAppendix :: Bool
                                 , shortChapterUri :: URI
                                 }

data UpdateChapter = UpdateChapter { updateChapterName :: T.Text
                                   , updateChapterIndex :: Int
                                   , updateChapterIsAppendix :: Bool
                                   }

data Live = Live | LiveAt UTCTime | Unscheduled | Completed | Cancelled

data Quest = Quest { questName :: T.Text
                   , questTeaser :: T.Text
                   , questBanner :: Maybe URI
                   , questVisibility :: Visibility
                   , questTags :: [Short Tag]
                   , questLive :: Live
                   , questWords :: Int
                   , questCreated :: UTCTime
                   , questChapters :: [Short Chapter]
                   }

data ShortQuest = ShortQuest { shortQuestName :: T.Text
                             , shortQuestTeaser :: T.Text
                             , shortQuestBanner :: Maybe URI
                             , shortQuestVisibility :: Visibility
                             , shortQuestTags :: [Short Tag]
                             , shortQuestLive :: Live
                             , shortQuestWords :: Int
                             , shortQuestCreated :: UTCTime
                             , shortQuestUri :: URI
                             }

data UpdateQuest = UpdateQuest { updateQuestName :: T.Text
                               , updateQuestTeaser :: T.Text
                               , updateQuestBanner :: Maybe URI
                               , updateQuestVisibility :: Visibility
                               , updateQuestTags :: [Short Tag]
                               , updateQuestLive :: Live
                               }

data QuestRole = QuestRole

instance RestApi Passage where
  type Short Passage = PassageContents
  type Update Passage = UpdatePassage
  type Create Passage = UpdatePassage
  type CaptureName Passage = "index"

instance RestApi Chapter where
  type Short Chapter = ShortChapter
  type Update Chapter = UpdateChapter
  type Create Chapter = UpdateChapter
  type CaptureName Chapter = "index"

instance RestApi Quest where
  type Short Quest = ShortQuest
  type Update Quest = UpdateQuest
  type Create Quest = UpdateQuest

instance RestApi QuestRole

instance ToSample Passage where
  toSamples _ = noSamples

instance ToSample PassageContents where
  toSamples _ = noSamples

instance ToSample UpdatePassage where
  toSamples _ = noSamples
  
instance ToSample Chapter where
  toSamples _ = noSamples

instance ToSample ShortChapter where
  toSamples _ = noSamples

instance ToSample UpdateChapter where
  toSamples _ = noSamples

instance ToSample Quest where
  toSamples _ = noSamples

instance ToSample ShortQuest where
  toSamples _ = noSamples

instance ToSample UpdateQuest where
  toSamples _ = noSamples

instance ToSample QuestRole where
  toSamples _ = noSamples

$(deriveJSON (jsonOptions "chapter") ''Chapter)
$(deriveJSON (jsonOptions "live") ''Live)
$(deriveJSON (jsonOptions "passage") ''Passage)
$(deriveJSON (jsonOptions "passageContents") ''PassageContents)
$(deriveJSON (jsonOptions "quest") ''Quest)
$(deriveJSON (jsonOptions "questRole") ''QuestRole)
$(deriveJSON (jsonOptions "shortChapter") ''ShortChapter)
$(deriveJSON (jsonOptions "shortQuest") ''ShortQuest)
$(deriveJSON (jsonOptions "updateChapter") ''UpdateChapter)
$(deriveJSON (jsonOptions "updatePassage") ''UpdatePassage)
$(deriveJSON (jsonOptions "updateQuest") ''UpdateQuest)
