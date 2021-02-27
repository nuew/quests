{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
module Network.Quests.Schema
        ()
where

import qualified Data.ByteString               as B
import qualified Data.Text                     as T
import           Data.Time
import qualified Database.PostgreSQL.Simple    as PQ
import           Opaleye
import           Data.Profunctor.Product
import           Data.Profunctor.Product.TH     ( makeAdaptorAndInstance )

