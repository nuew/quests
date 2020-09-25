{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
module Network.Quests.API.Polls
  ( Poll
  , Choice
  )
where

import           Data.Aeson.TH
import qualified Data.Text                     as T
import           Data.Time
import           Network.Quests.API.JSON
import           Network.Quests.API.Common
import           Network.Quests.API.Users
import           Servant.Docs

data PollSystem = PollSystemApproval
                | PollSystemBorda
                | PollSystemFirstPastThePost
                | PollSystemSchulze
                | PollSystemSingleTransferableVote

data Poll = Poll { pollSystem :: PollSystem
                 , pollPrompt :: T.Text
                 , pollOpen :: Bool
                 , pollWinners :: Int
                 , pollSubmissionsAllowed :: Bool
                 }

data Choice = Choice { choiceText :: T.Text
                     , choiceCancelled :: Maybe T.Text
                     , choiceCreatedAt :: UTCTime
                     , choiceCreatedBy :: Maybe (Ref User)
                     , choiceLastUpdatedAt :: Maybe UTCTime
                     , choiceLastUpdatedBy :: Maybe (Ref User)
                     }

instance RestApi Poll
instance RestApi Choice

instance ToSample Poll where
  toSamples _ = noSamples

instance ToSample Choice where
  toSamples _ = noSamples

$(deriveJSON (jsonOptions "choice") ''Choice)
$(deriveJSON (jsonOptions "poll") ''Poll)
$(deriveJSON (jsonOptions "pollSystem") ''PollSystem)
