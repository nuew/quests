{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}
module Network.Quests.Docs where

import           Control.Lens                  ((^.))
import qualified Data.ByteString.Lazy          as BL
import           Data.HashMap.Strict           (toList)
import qualified Data.Text                     as T
import qualified Data.Text.Encoding            as T
import           Data.List                     (sort)
import           Data.List.NonEmpty            (NonEmpty((:|)))
import           Network.HTTP.Media            ((//), (/:))
import           Servant.API.ContentTypes
import           Servant.Docs                  as SD
import           Text.Blaze.Html               as BZ
import           Text.Blaze.Html.Renderer.Utf8 as BZ
import           Text.Blaze.XHtml5             as BZ
import           Text.Blaze.XHtml5.Attributes  as BA

-- Render the body of the documentation/the actual API docs
renderApi :: API -> BZ.Html
renderApi = renderEndpoints
  where
    showPath sep [] = [sep]
    showPath sep el = concatMap (sep :) el

    getEndpoints api = sort . toList $ api ^. apiEndpoints

    methodClasses = BZ.textValue . T.append (T.pack "method ")
    endpointId http_method = BZ.textValue . T.append http_method . T.pack . showPath '.'

    renderCapture capture = BZ.li $ do
      BZ.h4 ! BA.class_ "sym" $ BZ.toHtml $ capture ^. capSymbol
      BZ.p . BZ.toHtml $ capture ^. capDesc
    renderCaptures [] = textComment "no captures for this endpoint"
    renderCaptures act_captures = BZ.section ! BA.class_ "captures" $
      BZ.h3 "Captures" >> BZ.ul (mconcat $ fmap renderCapture act_captures)
    htmlOfEndpoint endpoint ep_action = 
      -- this is known to be safe as the only methods that'll appear here are ones we've cerated
      let http_method = T.decodeUtf8 $ endpoint ^. SD.method 
          http_method_lower = T.toLower http_method in 
      BZ.article ! BA.class_ (BZ.textValue http_method_lower) 
                 ! BA.id (endpointId http_method_lower $ endpoint ^. path)
      $ do
        -- path
        BZ.h2 . BZ.toHtml . showPath '/' $ endpoint ^. path

        -- method
        BZ.span ! BA.class_ (methodClasses http_method_lower) $ BZ.toHtml http_method

        -- notes
        -- auth
        renderCaptures $ ep_action ^. captures  -- captures
        -- headers
        -- params
        -- fragment
        -- rqbody
        -- response

    renderEndpoints = mconcat . fmap (uncurry htmlOfEndpoint) . getEndpoints

-- Render the Documentation
renderDocs :: API -> BZ.Html
renderDocs = renderPage . renderApi
  where
    -- URLs/etc. used in page
    metaViewportContent = "width=device-width,initial-scale=1"
    shortcutIconUrl = "https://nuew.net/theme/favicon.ico"
    cssUrl = "https://nuew.net/theme/main.css"
    documentTitle = "API Documentation"

    -- Page Header Information
    pageMetaCharset = BZ.meta ! BA.charset "utf-8"
    pageMetaViewport = BZ.meta ! BA.name "viewport" ! BA.content metaViewportContent
    pageShortcut = BZ.link ! BA.rel "shortcut icon" ! BA.href shortcutIconUrl
    pageCss = BZ.link ! BA.rel "stylesheet" ! BA.type_ "text/css" ! BA.href cssUrl
    pageTitle = BZ.title documentTitle
    pageHeader = BZ.head $ pageMetaCharset >> pageMetaViewport >>
      pageShortcut >> pageCss >> pageTitle

    renderPage page = pageHeader >> (BZ.body $ do
      BZ.h1 documentTitle
      BZ.main page)

data XHTML -- XHTML Rendering

instance Accept XHTML where
  contentTypes _ = "application" // "xhtml+xml" /: ("charset", "utf-8") :| 
                   ["application" // "xhtml+xml"]

instance MimeRender XHTML API where
  mimeRender _ = BZ.renderHtml . dtHtmlAttrs . renderDocs
    where dtHtmlAttrs = BZ.docTypeHtml ! BA.xmlns "http://www.w3.org/1999/xhtml"

data HTML -- HTML Rendering (mostly for older browsers, just for safety, etc.)

instance Accept HTML where
  contentTypes _ = "text" // "html" /: ("charset", "utf-8") :|
                  ["text" // "html"]

instance MimeRender HTML API where
  mimeRender _ = BZ.renderHtml . (BZ.docTypeHtml ! BA.lang "en-US") . renderDocs

data Markdown -- Markdown/Plain Text Rendering

instance Accept Markdown where
  contentTypes _ = "text" // "markdown" /: ("charset", "utf-8") /: ("variant", "CommonMark") :|
                   [ "text" // "markdown" /: ("charset", "utf-8")
                   , "text" // "plain" /: ("charset", "utf-8")
                   ]

instance MimeRender Markdown API where
  mimeRender _ = BL.fromStrict . T.encodeUtf8 . T.pack . markdown
