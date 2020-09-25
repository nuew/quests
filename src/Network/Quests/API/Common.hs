{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
module Network.Quests.API.Common where

import           Data.Aeson.TH
import qualified Data.ByteString               as B
import           Data.Int
import qualified Data.Text                     as T
import qualified Database.PostgreSQL.Simple    as PG
import           GHC.TypeLits
import           Network.URI
import           Network.Quests.API.JSON
import           Servant
import           Servant.Docs

class RestApi a where
  type Ref a :: *
  type Ref a = URI

  type Short a :: *
  type Short a = Ref a

  type Create a :: *
  type Create a = a

  type Update a :: *
  type Update a = a

  type CaptureName a :: Symbol
  type CaptureName a = "id"

  type CaptureType a :: *
  type CaptureType a = Int32

data Visibility = Public | Unlisted | Private
  deriving (Eq, Ord, Enum, Bounded)

instance ToCapture (Capture "id" Int32) where
  toCapture _ =
    DocCapture "id" "The numeric identifier that corresponds to the resource."

instance ToCapture (Capture "index" Int32) where
  toCapture _ =
    DocCapture "index" "The index of the resource within its container."

instance ToCapture (Capture "slug" T.Text) where
  toCapture _ = DocCapture "slug" "The slug that corresponds to the resource."

instance ToCapture (Capture "tag" T.Text) where
  toCapture _ = DocCapture "tag" "The name of the tag."

instance ToHttpApiData URI where
  toUrlPiece = uriToText

instance ToSample URI where
  toSamples _ =
    singleSample $ URI "https:" (Just $ URIAuth "" "example.com" "") "/" "" ""

instance ToSample T.Text where
  toSamples _ = singleSample "Lorem ipsum dolor sit amet"

$(deriveJSON (jsonOptions "") ''Visibility)
