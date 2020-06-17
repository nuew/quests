{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
module Network.Quests.API.Common where

import           Data.Aeson.Encoding
import           Data.Aeson.Types
import qualified Data.ByteString               as B
import           Data.Char
import           Data.List
import           Data.Maybe
import qualified Data.Text                     as T
import           Network.URI
import           Servant
import           Servant.Docs

class RestApi a where
     type Short a :: *
     type Short a = a

     type Create a :: *
     type Create a = a

     type Update a :: *
     type Update a = a

type RestCollection a =
     Get '[JSON] [a] :<|>
     ReqBody '[JSON] (Create a) :> PostCreated '[JSON] (Headers '[Header "Location" URI] a)

type RestObject a =
     Get '[JSON] a :<|>
     ReqBody '[JSON] (Update a) :> Put '[JSON] a :<|>
     DeleteNoContent '[JSON] NoContent

type SimpleRestApi a = RestCollection a :<|> RestObject a
type IdRestApi a = Capture "id" Integer :> SimpleRestApi a
type SlugRestApi a = Capture "slug" T.Text :> SimpleRestApi a

instance FromJSON URI where
  parseJSON = withText "URI" $ failNothing . parseURI . T.unpack
    where failNothing = maybe (fail "couldn't parse as URI") return

instance ToCapture (Capture "id" Integer) where
  toCapture _ =
    DocCapture "id" "The numeric identifier that corresponds to the resource."

instance ToCapture (Capture "slug" T.Text) where
  toCapture _ = DocCapture "slug" "The slug that corresponds to the resource."

uriToText uri = T.pack $ uriToString id uri ""

instance ToHttpApiData URI where
  toUrlPiece = uriToText

instance ToJSON URI where
  toJSON = String . uriToText
  toEncoding = text . uriToText

instance ToSample URI where
  toSamples _ =
    singleSample $ URI "https:" (Just $ URIAuth "" "example.com" "") "/" "" ""

instance ToSample T.Text where
  toSamples _ = singleSample "Lorem ipsum dolor sit amet"

jsonOptions :: String -> Options
jsonOptions prefix = defaultOptions { fieldLabelModifier = stripPrefixL prefix
                                    , constructorTagModifier = stripPrefixU prefix
                                    , unwrapUnaryRecords = True
                                    }
    where
      mapFirst f (x:xs) = f x : xs
      mapFirst f [] = []

      stripPrefixL prefix = (mapFirst toLower) . fromJust . stripPrefix prefix
      stripPrefixU = stripPrefixL . mapFirst toUpper
