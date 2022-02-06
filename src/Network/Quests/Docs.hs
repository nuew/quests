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
import           Data.List                     as L
import           Data.List.NonEmpty            (NonEmpty((:|)), groupWith)
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
    -- Misc Helper functions
    showPath sep [] = [sep]
    showPath sep el = concatMap (sep :) el
    emptyMarkup = BZ.toMarkup ([] :: [BZ.Markup])
    endpointId http_method = BZ.textValue . T.append http_method . T.pack . showPath '.'
    methodClasses = BZ.textValue . T.append "method "
    statusClasses = BZ.stringValue . (++) "status http"

    containerSection = (!) BZ.section . BA.class_ . BZ.textValue . T.toLower
    renderContainer c_name = containerSection c_name . (>>) (BZ.h3 . BZ.toHtml $ c_name)
    renderContainerSimple c_name mapfn = renderContainer c_name . mconcat . fmap mapfn

    -- Render the notes of a single API endpoint
    renderNote note = BZ.section ! BA.class_ "note" $ do
      BZ.h4 . BZ.string $ note ^. noteTitle
      mconcat . fmap (BZ.p . BZ.string) $ note ^. noteBody
    renderNotes [] = emptyMarkup
    renderNotes act_notes = renderContainerSimple "Notes" renderNote act_notes

    -- Render the documentation of the captures of a single API endpoint
    renderCapture capture = do
      (BZ.dt . BZ.string $ capture ^. capSymbol)
      (BZ.dd . BZ.string $ capture ^. capDesc)
    renderCaptures [] = emptyMarkup
    renderCaptures act_captures = renderContainer "Captures" $
      BZ.dl (mconcat $ fmap renderCapture act_captures)

    -- Render the parameters of a single API endpoint
    renderParam _ = errorWithoutStackTrace "unimplemented"
    renderParams [] = emptyMarkup
    renderParams act_params = renderContainerSimple "Params" renderParam act_params

    -- Render the response details for a single API endpoint
    renderResponseBody (rb_title, rb_mime, rb_content) = BZ.section ! BA.class_ "resp-example" $ do
      BZ.h4 $ do
        BZ.text "Example ("
        mconcat . L.intersperse ", " . fmap (BZ.code . BZ.string . show) $ rb_mime
        BZ.text ")"
      BZ.pre . BZ.code . BZ.text . T.decodeUtf8 . BL.toStrict $ rb_content
    mergeResponseBodies = foldr (\(ti, m1, co) (_, m2, _) -> (ti, m1 : m2, co))
      (("" :: T.Text), [], BL.empty) -- base case; we don't need to care about the title/content
                                     -- we'll just replace it every time.
    renderResponseBodies [] = emptyMarkup
    renderResponseBodies el = mconcat $ fmap (renderResponseBody . mergeResponseBodies) el
    renderResponse act_response = renderContainer "Response" $ do
      let status = show $ act_response ^. respStatus
      BZ.span ! BA.class_ (statusClasses status) $ BZ.string status 
      BZ.section ! BA.class_ "content-types" $ do
        BZ.h4 $ BZ.string "Supported Content Types"
        BZ.ul . mconcat . fmap (BZ.li . BZ.code . BZ.string . show) $ act_response ^. respTypes
      renderResponseBodies . groupWith (\(ti, _, co) -> (ti, co)) $ act_response ^. respBody

    -- Render a single API endpoint's documentation
    htmlOfEndpoint endpoint ep_action = 
      -- this is known to be safe as the only methods that'll appear here are ones we've created
      let http_method = T.decodeUtf8 $ endpoint ^. SD.method 
          http_method_lower = T.toLower http_method in 
      BZ.article ! BA.class_ (BZ.textValue http_method_lower) 
                 ! BA.id (endpointId http_method_lower $ endpoint ^. path)
      $ do
        -- path
        BZ.h2 . BZ.string . showPath '/' $ endpoint ^. path

        -- method
        BZ.span ! BA.class_ (methodClasses http_method_lower) $ BZ.text http_method

        renderNotes $ ep_action ^. notes -- notes
        -- auth
        renderCaptures $ ep_action ^. captures  -- captures
        -- headers
        renderParams $ ep_action ^. params -- params
        -- fragment
        -- rqbody
        renderResponse $ ep_action ^. response -- response

    getEndpoints api = L.sort . toList $ api ^. apiEndpoints
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
