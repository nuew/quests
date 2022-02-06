module Network.Quests.GlobalContext
  ( ContextConfiguration(..)
  , GlobalContext(..)
  , setupGlobalContext
  , destroyGlobalContext
  )
where

import           Crypto.Random
import qualified Data.ByteString               as B
import           Data.Pool
import           Data.Time.Clock

type DBConnection = ()

data ContextConfiguration = ContextConfiguration
  { databaseConnection :: B.ByteString
  , databasePoolMaxConns :: Int
  , databasePoolStripes :: Int
  , databasePoolTimeout :: NominalDiffTime
  }

data GlobalContext = GlobalContext
  { databasePool :: Pool DBConnection
  , rngPool :: Pool ChaChaDRG
  }

setupDatabase :: ContextConfiguration -> IO (Pool DBConnection)
setupDatabase cfg = createPool (return ())
                               (\_ -> return ())
                               (databasePoolStripes cfg)
                               (databasePoolTimeout cfg)
                               (databasePoolMaxConns cfg)

setupRng :: ContextConfiguration -> IO (Pool ChaChaDRG)
setupRng _ = createPool drgNew
                        (\_ -> return ())
                        1
                        (1 :: NominalDiffTime)
                        1

setupGlobalContext :: ContextConfiguration -> IO GlobalContext
setupGlobalContext cfg = do
  databasePool' <- setupDatabase cfg
  rngPool' <- setupRng cfg
  return GlobalContext { databasePool = databasePool'
                       , rngPool = rngPool'
                       }

destroyGlobalContext :: GlobalContext -> IO ()
destroyGlobalContext gctx = do
    destroyAllResources $ databasePool gctx
    destroyAllResources $ rngPool gctx
