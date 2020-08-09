{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
module Network.Quests.Server
        ( apiV1Server
        )
where

import           Control.Exception.Base
import           Control.Monad.IO.Class
import           Crypto.Argon2
import           Data.Pool
import qualified Data.ByteString               as B
import qualified Data.ByteString.Lazy          as BL
import qualified Data.Text                     as T
import qualified Data.Text.Encoding            as T
import           Data.Time.Clock
import           Data.Maybe
import qualified Database.PostgreSQL.Simple    as PG
import qualified Database.PostgreSQL.Simple.Time as PG
import           Network.Quests.API
import           Network.Quests.API.Users
import           Network.URI
import           Servant
import           Servant.Links
import           System.Entropy

mapSqlError :: PG.SqlError -> ServerError
mapSqlError e = err422 { errBody = BL.fromStrict $ PG.sqlErrorDetail e }

unimplemented :: Handler a
unimplemented = throwError err501

bookshelvesServer :: Pool PG.Connection -> Server BookshelvesApi
bookshelvesServer _ = undefined

chatsServer :: Pool PG.Connection -> Server ChatsApi
chatsServer _ = undefined

pollsServer :: Pool PG.Connection -> Server PollsApi
pollsServer _ = undefined

questsServer :: Pool PG.Connection -> Server QuestsApi
questsServer _ = undefined

reportsServer :: Pool PG.Connection -> Server ReportsApi
reportsServer _ = undefined

tagsServer :: Pool PG.Connection -> Server TagsApi
tagsServer _ = undefined

userDetailLocationProxy :: Proxy ("v1"
                        :> "users"
                        :> Capture "slug" T.Text
                        :> Get '[JSON] User)
userDetailLocationProxy = Proxy

searchUsersServer :: Pool PG.Connection
                  -> Server (Get '[JSON] [ShortUser])
searchUsersServer pool = liftIO . withResource pool $ \conn ->
    PG.fold_ conn "SELECT DISTINCT ON (u.id)\
                  \ u.name, l.slug, u.avatar, l.created_at, s.last_active\
                  \ FROM users u\
                  \ INNER JOIN user_slugs l ON u.id = l.user_id\
                  \ LEFT JOIN sessions s ON u.id = s.user_id\
                  \ ORDER BY u.id ASC, s.last_active DESC, l.created_at DESC;"
                  []
                  (\a -> return . prependUser a)
  where
    prependUser :: [ShortUser]
                -> (T.Text, T.Text, Maybe String, UTCTime, Maybe UTCTime)
                -> [ShortUser]
    prependUser a t = shortUserFromTuple t : a
    shortUserFromTuple (name, slug, avatar, slug_created, last_active) =
      ShortUser name
                (avatar >>= parseURI)
                (fromMaybe slug_created last_active)
                (linkURI $ safeLink api userDetailLocationProxy slug)

newUserServer :: Pool PG.Connection
              -> CreateUser
              -> Server (PostCreated '[JSON] (Headers '[Header "Location" URI] User))
newUserServer pool uc = liftIO . withResource pool $ \conn -> do
    salt <- getEntropy 32 -- FIXME use a vault crypto rng if possible
    let slug = createUserName uc
    db_time <- catch (createNewUser conn uc slug salt) (\e -> throw $ mapSqlError e)

    -- generate return value
    let newUser = User { userName = createUserName uc
                       , userEmail = createUserEmail uc
                       , userAvatar = Nothing
                       , userCreated = db_time
                       , userLastActive = db_time
                       , userBiography = ""
                       , userLocation = ""
                       , userPronouns = ""
                       , userWebsite = ""
                       , userBookshelves = []
                       , userFollowers = []
                       , userFollowing = []
                       , userQuests = []
                       }

    let newUserLocation = safeLink api userDetailLocationProxy slug
    return $ addHeader (linkURI newUserLocation) $ newUser
  where
    createNewUser :: PG.Connection -> CreateUser -> T.Text -> B.ByteString -> IO UTCTime
    createNewUser conn uc slug salt = PG.withTransaction conn $ do
      let utf8EncodedPassword = T.encodeUtf8 $ createUserPassword uc
          Right pw_hash = hash defaultHashOptions utf8EncodedPassword salt

      [(db_id, PG.Finite db_time)] <- PG.query conn
        "INSERT INTO users (name, email, password_hash)\
        \ VALUES (?, ?, ?) RETURNING id, created_at;"
        (createUserName uc, createUserEmail uc, PG.Binary pw_hash)
      PG.execute conn
        "INSERT INTO user_slugs (slug, user_id) VALUES (?, ?);"
        (slug, db_id :: Int)

      return db_time

userDetailServer :: Pool PG.Connection -> T.Text -> Server (Get '[JSON] User)
userDetailServer pool name = liftIO . withResource pool $ \conn -> do
    [row] <- PG.query conn "SELECT id, name, email, avatar, created_at,\
                           \ last_active, biography, location, pronouns,\
                           \ website\
                           \ FROM users_long u\
                           \ WHERE slug = ?\
                           \ ORDER BY last_active DESC, created_at DESC\
                           \ LIMIT 1;"
                           $ PG.Only name
    -- todo get bs, follow(s|ers), quests
    let (id, name, email, avatar, created_at, last_active, biography, location,
         pronouns, website) = row
        _ = id :: Int
    return User { userName = name
                , userEmail = email
                , userAvatar = avatar >>= parseURI
                , userCreated = created_at
                , userLastActive = fromMaybe created_at last_active
                , userBiography = biography
                , userLocation = location
                , userPronouns = pronouns
                , userWebsite = website
                , userBookshelves = bookshelvesOf id
                , userFollowers = followersOf id
                , userFollowing = followingOf id
                , userQuests = questsOf id
                }
  where
    bookshelvesOf id = []
    followersOf id = []
    followingOf id = []
    questsOf id = []

usersServer :: Pool PG.Connection -> Server UsersApi
usersServer pool = searchUsersServer pool
              :<|> newUserServer pool
              :<|> specificUserServer
  where
    specificUserServer name = userDetailServer pool name
                         :<|> userUpdate name
                         :<|> userDelete name
                         :<|> userSessions name
                         :<|> userBans name

    userUpdate name update = unimplemented
    userDelete name = unimplemented
    userSessions name = undefined
    userBans name = undefined

apiV1Server :: Pool PG.Connection -> Server ApiVersion1
apiV1Server pool = bookshelvesServer pool
              :<|> chatsServer pool
              :<|> pollsServer pool
              :<|> questsServer pool
              :<|> reportsServer pool
              :<|> tagsServer pool
              :<|> usersServer pool
              :<|> undefined
