module Network.Quests.API.JSON where

import           Data.Aeson.TH
import           Data.Aeson.Types
import           Data.Char
import           Data.List
import           Data.Maybe
import qualified Data.Text                     as T
import           Network.URI

jsonOptions :: String -> Options
jsonOptions prefix = defaultOptions
  { fieldLabelModifier     = stripPrefixL prefix
  , constructorTagModifier = stripPrefixU prefix
  , unwrapUnaryRecords     = True
  }
 where
  mapFirst f (x : xs) = f x : xs
  mapFirst _ []       = []

  stripPrefixL prefixL = mapFirst toLower . fromMaybe "" . stripPrefix prefixL
  stripPrefixU = stripPrefixL . mapFirst toUpper

-- This is evil, but there's not much else to do here, given Crypto.JOSE.Types.Orphans. In this
-- case, we try to match behaivour exactly, so it shouldn't matter which instance is chosen.
instance {-# INCOHERENT #-} FromJSON URI where
  parseJSON = withText "URI" (maybe (fail "not a URI") return . parseURI . T.unpack)

instance {-# INCOHERENT #-} ToJSON URI where
  toJSON = String . T.pack . show
