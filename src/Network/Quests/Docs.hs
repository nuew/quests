{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}
module Network.Quests.Docs where

import           CMark
import qualified Data.Text                     as T
import           Data.List.NonEmpty
import           Network.HTTP.Media            ((//), (/:))
import           Servant.API.ContentTypes
import           Text.Blaze.Html
import           Text.Blaze.Html.Renderer.Utf8
import           Text.Blaze.XHtml5             as BZ
import           Text.Blaze.XHtml5.Attributes

h5render :: T.Text -> Html
h5render docs = apiHeader >> apiBody docs
  where
    cmarkOptions = [optNormalize, optSmart, optUnsafe]

    cssLink url = BZ.link ! rel "stylesheet" ! type_ "text/css" ! href url
    shortcutLink url = BZ.link ! rel "shortcut icon" ! href url

    apiShortcut = shortcutLink "https://nuew.net/theme/favicon.ico"
    apiCss = cssLink "https://nuew.net/theme/main.css"
    apiTitle = BZ.title "API Documentation"
    apiHeader = BZ.head $ apiShortcut >> apiCss >> apiTitle

    apiDocument = preEscapedToHtml . commonmarkToHtml cmarkOptions
    apiBody = BZ.body . apiDocument

dtHtmlEnUS :: Html -> Html
dtHtmlEnUS = docTypeHtml ! lang "en-US"

data XHTML

instance Accept XHTML where
  contentTypes _ = "application" // "xhtml+xml" /: ("charset", "utf-8") :| 
                   ["application" // "xhtml+xml"]

instance MimeRender XHTML T.Text where
  mimeRender _ = renderHtml . (dtHtmlEnUS ! xmlns "http://www.w3.org/1999/xhtml") . h5render 

data HTML

instance Accept HTML where
  contentTypes _ = "text" // "html" /: ("charset", "utf-8") :|
                  ["text" // "html"]

instance MimeRender HTML T.Text where
  mimeRender _ = renderHtml . dtHtmlEnUS . h5render
