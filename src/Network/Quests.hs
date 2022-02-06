{-# LANGUAGE DataKinds #-}
module Network.Quests
  ( ContextConfiguration(..)
  , app
  , applyMigrations
  , dumpLayout
  )
where

import           Control.Exception.Base
import qualified Data.ByteString               as B
import qualified Data.Text                     as T
import           Network.Quests.API
import           Network.Quests.GlobalContext
import           Network.Quests.Server
import           Servant
import           Servant.Auth.Server

server :: GlobalContext -> Server ApiRoot
server gctx = return apiDocs :<|> apiV1Server gctx

apiContext :: B.ByteString -> Context '[JWTSettings, CookieSettings]
apiContext key = jwtCtx :. defaultCookieSettings :. EmptyContext
  where jwtCtx = defaultJWTSettings $ fromSecret key

app :: ContextConfiguration -> B.ByteString -> IO Application
app cfg key = bracket (setupGlobalContext cfg) destroyGlobalContext
  $ return . serveWithContext api (apiContext key) . server

applyMigrations :: ContextConfiguration -> IO ()
applyMigrations _ = return ()

dumpLayout :: B.ByteString -> String
dumpLayout = T.unpack . layoutWithContext api . apiContext
