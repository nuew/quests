module Network.Quests.API.JSON where

import           Data.Aeson.Encoding
import           Data.Aeson.TH
import           Data.Aeson.Types
import           Data.Char
import           Data.List
import           Data.Maybe
import qualified Data.Text                     as T
import           Network.URI
import           Servant.Auth.Server

uriToText uri = T.pack $ uriToString id uri ""

jsonOptions :: String -> Options
jsonOptions prefix = defaultOptions
  { fieldLabelModifier     = stripPrefixL prefix
  , constructorTagModifier = stripPrefixU prefix
  , unwrapUnaryRecords     = True
  }
 where
  mapFirst f (x : xs) = f x : xs
  mapFirst f []       = []

  stripPrefixL prefix = mapFirst toLower . fromMaybe "" . stripPrefix prefix
  stripPrefixU = stripPrefixL . mapFirst toUpper
