module Network.Quests.API.JSON where

import           Data.Aeson.Encoding
import           Data.Aeson.TH
import           Data.Aeson.Types
import           Data.Char
import           Data.List
import           Data.Maybe
import qualified Data.Text                     as T
import           Network.URI

uriToText uri = T.pack $ uriToString id uri ""

jsonOptions :: String -> Options
jsonOptions prefix = defaultOptions { fieldLabelModifier = stripPrefixL prefix
                                    , constructorTagModifier = stripPrefixU prefix
                                    , unwrapUnaryRecords = True
                                    }
    where
      mapFirst f (x:xs) = f x : xs
      mapFirst f [] = []

      stripPrefixL prefix = mapFirst toLower . fromJust . stripPrefix prefix
      stripPrefixU = stripPrefixL . mapFirst toUpper

instance FromJSON URI where
  parseJSON = withText "URI" $ failNothing . parseURI . T.unpack
    where failNothing = maybe (fail "couldn't parse as URI") return

instance ToJSON URI where
  toJSON = String . uriToText
  toEncoding = text . uriToText
