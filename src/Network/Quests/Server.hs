{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
module Network.Quests.Server
        ( apiV1Server
        )
where

import           Control.Monad.IO.Class
import           Crypto.Argon2
import           Data.Pool
import qualified Data.Text                     as T
import qualified Data.Text.Encoding            as T
import qualified Database.PostgreSQL.Simple    as PG
import qualified Database.PostgreSQL.Simple.Time as PG
import           Network.Quests.API
import           Network.Quests.API.Users
import           Servant
import           Servant.Links
import           System.Entropy

bookshelvesServer :: Pool PG.Connection -> Server BookshelvesApi
bookshelvesServer = undefined

chatsServer :: Pool PG.Connection -> Server ChatsApi
chatsServer = undefined

pollsServer :: Pool PG.Connection -> Server PollsApi
pollsServer = undefined

questsServer :: Pool PG.Connection -> Server QuestsApi
questsServer = undefined

tagsServer :: Pool PG.Connection -> Server TagsApi
tagsServer = undefined

usersServer :: Pool PG.Connection -> Server UsersApi
usersServer pool = searchUsers :<|> newUser :<|> specificUser
  where
    searchUsers = undefined

    newUserLocationProxy = Proxy :: Proxy ("v1" :> "users" :> Capture "slug" T.Text :> Get '[JSON] User)
    newUser uc = liftIO . withResource pool $ \conn -> do
        -- password and slug munging
        salt <- getEntropy 32
        let Right pw_hash = hash defaultHashOptions (T.encodeUtf8 $ createUserPassword uc) salt
        let slug = createUserName uc

        -- do the database
        [[db_id]] <- PG.query conn "BEGIN;\

                                   \INSERT INTO users (name, email, password_hash)\
                                   \VALUES (?, ?, ?) RETURNING id;"
            ( createUserName uc
            , createUserEmail uc
            , PG.Binary pw_hash
            )
        PG.execute conn "INSERT INTO user_slugs (slug, user_id) VALUES (?, ?); COMMIT;" (slug, db_id :: Int)
        [[PG.Finite db_time]] <- PG.query conn "SELECT created_at FROM users WHERE id=?;" (PG.Only db_id)

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
                        
        let newUserLocation = safeLink api newUserLocationProxy slug
        return $ addHeader (linkURI newUserLocation) $ newUser
        
    specificUser name = userDetail name
                   :<|> userUpdate name
                   :<|> userDelete name
                   :<|> userSessions name
                   :<|> userBans name

    userDetail name = undefined
    userUpdate name update = undefined
    userDelete name = undefined
    userSessions name = undefined
    userBans = undefined

apiV1Server :: Pool PG.Connection -> Server ApiVersion1
apiV1Server pool = bookshelvesServer pool
              :<|> chatsServer pool
              :<|> pollsServer pool
              :<|> questsServer pool
              :<|> tagsServer pool
              :<|> usersServer pool
              :<|> undefined
