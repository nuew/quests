{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}
module Network.Quests.Docs where

import           Control.Lens                  ((^.))
import qualified Data.ByteString.Lazy          as BL
import           Data.CaseInsensitive          as CI
import           Data.HashMap.Strict           (toList)
import qualified Data.Text                     as T
import qualified Data.Text.Encoding            as T
import           Data.List                     as L
import           Data.List.NonEmpty            (NonEmpty((:|)), groupWith)
import           Network.HTTP.Media            ((//), (/:), subType)
import           Servant.API.ContentTypes
import           Servant.Docs                  as SD
import           Text.Blaze.Html               as BZ
import           Text.Blaze.Html.Renderer.Utf8 as BZ
import           Text.Blaze.XHtml5             as BZ
import           Text.Blaze.XHtml5.Attributes  as BA

emptyMarkup :: Html
emptyMarkup = BZ.toMarkup ([] :: [BZ.Markup])

-- Render the body of the documentation/the actual API docs
renderApi :: API -> BZ.Html
renderApi = renderEndpoints
  where
    -- Misc Helper functions
    showPath sep [] = [sep]
    showPath sep el = concatMap (sep :) el
    endpointId http_method = BZ.textValue . T.append http_method . T.pack . showPath '.'
    methodClasses = BZ.textValue . T.append "method "
    statusClasses = BZ.stringValue . (++) "status http"

    containerSection = (!) BZ.section . BA.class_ . BZ.textValue . T.toLower
    renderContainer c_name = containerSection c_name . (>>) (BZ.h3 . BZ.toHtml $ c_name)
    renderContainerSimple c_name mapfn = renderContainer c_name . mconcat . fmap mapfn

    languageClassMap "xhtml+xml" = "xml"
    languageClassMap mime_name = mime_name
    taggedCodeBlock mime = BZ.pre . (BZ.code ! (BA.class_ . BZ.textValue .
      T.append "language-" . languageClassMap . T.decodeUtf8 . foldedCase $ subType mime))

    -- Request/Response details for a single API endpoint
    renderContentTypes [] = emptyMarkup
    renderContentTypes content_types = BZ.section ! BA.class_ "content-types" $ do
        BZ.h4 $ BZ.string "Supported Content Types"
        BZ.ul . mconcat $ Prelude.map (BZ.li . BZ.code . BZ.string . show) content_types

    renderRqRspBody (rb_title, rb_mime, rb_content) = BZ.section ! BA.class_ "code-example" $ do
      BZ.h4 $ do
        BZ.text "Example ("
        mconcat . L.intersperse ", " . Prelude.map (BZ.code . BZ.string . show) $ rb_mime
        BZ.text ")"
      taggedCodeBlock (Prelude.head rb_mime) . BZ.text . T.decodeUtf8 . BL.toStrict $ rb_content
    mergeRqRspBodies = foldr (\(ti, m1, co) (_, m2, _) -> (ti, m1 : m2, co))
      (("" :: T.Text), [], BL.empty) -- base case; we don't need to care about the title/content
                                     -- we'll just replace it every time.
    renderRqRspBodies [] = emptyMarkup
    renderRqRspBodies el = mconcat $ fmap (renderRqRspBody . mergeRqRspBodies) el

    -- Render the notes of a single API endpoint
    renderNote note = BZ.section ! BA.class_ "note" $ do
      BZ.h4 . BZ.string $ note ^. noteTitle
      mconcat . fmap (BZ.p . BZ.string) $ note ^. noteBody
    renderNotes [] = emptyMarkup
    renderNotes act_notes = renderContainerSimple "Notes" renderNote act_notes
    
    -- Render the authentication details of a single API endpoint
    renderAuths [] = emptyMarkup
    renderAuths _ = errorWithoutStackTrace "renderAuths unimplemented"

    -- Render the documentation of the captures of a single API endpoint
    renderCapture capture = do
      (BZ.dt . BZ.string $ capture ^. capSymbol)
      (BZ.dd . BZ.string $ capture ^. capDesc)
    renderCaptures [] = emptyMarkup
    renderCaptures act_captures = renderContainer "Captures" $
      BZ.dl (mconcat $ fmap renderCapture act_captures)

    -- Render the headers of a single API endpoint
    renderHeaders [] = emptyMarkup
    renderHeaders _ = errorWithoutStackTrace "renderHeaders unimplemented"

    -- Render the parameters of a single API endpoint
    renderParams [] = emptyMarkup
    renderParams _ = errorWithoutStackTrace "renderParams unimplemented"

    -- Render the request details of a single API endpoint
    renderRequest [] [] = emptyMarkup
    renderRequest act_mimes act_bodies = renderContainer "Request" $ do
      renderContentTypes act_mimes  
      renderRqRspBodies $ groupWith (\(ti, _, co) -> (ti, co)) act_bodies

    -- Render the response details of a single API endpoint
    renderResponse act_response = renderContainer "Response" $ do
      let status = show $ act_response ^. respStatus
      BZ.span ! BA.class_ (statusClasses status) $ BZ.string status
      renderContentTypes $ act_response ^. respTypes
      renderRqRspBodies . groupWith (\(ti, _, co) -> (ti, co)) $ act_response ^. respBody

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
        renderAuths $ ep_action ^. authInfo -- auth
        renderCaptures $ ep_action ^. captures  -- captures
        renderHeaders $ ep_action ^. SD.headers -- headers
        renderParams $ ep_action ^. params -- params
        -- fragment
        renderRequest (ep_action ^. rqtypes) $ ep_action ^. rqbody -- rqbody
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
    cssUrls = [ "https://nuew.net/theme/main.css"
              , "https://cdn.jsdelivr.net/npm/prismjs@1.26.0/themes/prism-solarizedlight.min.css"
              ]
    jsUrls = [ "https://cdn.jsdelivr.net/npm/prismjs@1.26.0/prism.min.js"
             , "https://cdn.jsdelivr.net/npm/prismjs@1.26.0/plugins/autoloader/prism-autoloader.min.js"
             ]
    documentTitle = "API Documentation"

    -- utility functions
    cssFor url = BZ.link ! BA.rel "stylesheet" ! BA.type_ "text/css" ! BA.href url
    jsFor url = BZ.script ! BA.type_ "application/javascript" ! BA.src url $ emptyMarkup

    -- Page Header Information
    pageMetaCharset = BZ.meta ! BA.charset "utf-8"
    pageMetaViewport = BZ.meta ! BA.name "viewport" ! BA.content metaViewportContent
    pageShortcut = BZ.link ! BA.rel "shortcut icon" ! BA.href shortcutIconUrl
    pageCss = mconcat . Prelude.map cssFor $ cssUrls
    pageJs = mconcat . Prelude.map jsFor $ jsUrls -- in header in spirit
    pageTitle = BZ.title documentTitle
    pageHeader = BZ.head $
      pageMetaCharset >> pageMetaViewport >> pageShortcut >> pageCss >> pageTitle

    pageDtHtml = BZ.docTypeHtml ! BA.xmlns "http://www.w3.org/1999/xhtml" ! BA.lang "en-US"
    renderPage page = pageDtHtml $ pageHeader >> (BZ.body $ do
      BZ.h1 documentTitle
      BZ.main page
      pageJs)

data XHTML -- XHTML Rendering

instance Accept XHTML where
  contentTypes _ = "application" // "xhtml+xml" /: ("charset", "utf-8") :| 
                   [ "application" // "xhtml+xml"
                   , "text" // "html" /: ("charset", "utf-8")
                   ,  "text" // "html"
                   ]

instance MimeRender XHTML API where
  mimeRender _ = BZ.renderHtml . renderDocs

data Markdown -- Markdown/Plain Text Rendering

instance Accept Markdown where
  contentTypes _ = "text" // "markdown" /: ("charset", "utf-8") /: ("variant", "CommonMark") :|
                   [ "text" // "markdown" /: ("charset", "utf-8")
                   , "text" // "plain" /: ("charset", "utf-8")
                   ]

instance MimeRender Markdown API where
  mimeRender _ = BL.fromStrict . T.encodeUtf8 . T.pack . markdown
