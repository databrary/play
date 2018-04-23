{-# LANGUAGE FlexibleInstances, FlexibleContexts, MultiParamTypeClasses, ConstraintKinds, DefaultSignatures, GeneralizedNewtypeDeriving, TypeFamilies, OverloadedStrings, StandaloneDeriving #-}
module Databrary.Service.DB
  ( DBPool
  , DBConn
  , initDB
  , finiDB
  , withDB
  , MonadDB
  , DBM
  , runDBM
  , liftDBM
  , dbTryJust
  , dbRunQuery
  , dbExecute
  , dbExecuteSimple
  , dbExecute1
  , dbExecute1'
  , dbExecute_
  , dbQuery
  , dbQuery1
  , dbQuery1'
  , dbTransaction
  , dbTransaction'
  , runDBConnection
  , useTDB
  , runTDB
  , mapQuery2
  ) where

import Control.Exception (tryJust, bracket)
import Control.Monad (unless)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Trans.Control (MonadBaseControl, liftBaseOp_)
import Control.Monad.Trans.Reader (ReaderT(..))
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Char8 as BS
import Data.IORef (IORef, newIORef, atomicModifyIORef')
import Data.Maybe (fromMaybe, isJust, fromJust)
import Data.Pool (Pool, withResource, createPool, destroyAllResources)
import qualified Database.PostgreSQL.Simple as PGSimple
import Database.PostgreSQL.Typed.Protocol
import Database.PostgreSQL.Typed.Query
import Database.PostgreSQL.Typed.TH (withTPGConnection, useTPGDatabase)
import Database.PostgreSQL.Typed.Types (PGValue)
import qualified Language.Haskell.TH as TH
import Network (PortID(..))
import System.IO.Unsafe (unsafePerformIO)

import Databrary.Has
import qualified Databrary.Store.Config as C

confPGDatabase :: C.Config -> PGDatabase
confPGDatabase conf = defaultPGDatabase
  { pgDBHost = fromMaybe "localhost" host
  , pgDBPort = if isJust host
     then PortNumber (maybe 5432 fromInteger $ conf C.! "port")
     else UnixSocket (fromJust $ conf C.! "sock")
  , pgDBName = fromMaybe user $ conf C.! "db"
  , pgDBUser = user
  , pgDBPass = fromMaybe "" $ conf C.! "pass"
  , pgDBDebug = fromMaybe False $ conf C.! "debug"
  }
  where
  host = conf C.! "host"
  user = conf C.! "user"


data DBPool = DBPool (Pool PGConnection) (Pool PGSimple.Connection)
type DBConn = PGConnection

initDB :: C.Config -> IO DBPool
initDB conf =
    DBPool
        <$> (createPool' (pgConnect db) pgDisconnect)
        <*> (createPool' (PGSimple.connect simpleConnInfo) (PGSimple.close))
  where
    createPool' :: IO a -> (a -> IO ()) -> IO (Pool a)
    createPool' get release =
        createPool get release stripes (fromInteger idle) conn
    db = confPGDatabase conf
    simpleConnInfo = PGSimple.defaultConnectInfo
        { PGSimple.connectHost     = pgDBHost db
        , PGSimple.connectPort     = case pgDBPort db of
            PortNumber x -> fromIntegral x -- x is opaque
            UnixSocket _ ->
                PGSimple.connectPort PGSimple.defaultConnectInfo
            Service _ ->
                PGSimple.connectPort PGSimple.defaultConnectInfo
        , PGSimple.connectUser     = BS.unpack (pgDBUser db)
        , PGSimple.connectPassword = BS.unpack (pgDBPass db)
        , PGSimple.connectDatabase = BS.unpack (pgDBName db)
        }
    stripes = fromMaybe 1 $ conf C.! "stripes"
    idle    = fromMaybe 300 $ conf C.! "idle"
    conn    = fromMaybe 16 $ conf C.! "maxconn"

finiDB :: DBPool -> IO ()
finiDB (DBPool p p') = do
   -- Different types -> no 'travers'ing
   destroyAllResources p
   destroyAllResources p'

withDB :: DBPool -> (DBConn -> IO a) -> IO a
withDB (DBPool p _) = withResource p

type MonadDB c m = (MonadIO m, MonadHas DBConn c m)

{-# INLINE liftDB #-}
liftDB :: MonadDB c m => (PGConnection -> IO a) -> m a
liftDB = focusIO

type DBM a = ReaderT PGConnection IO a

runDBM :: DBPool -> DBM a -> IO a
runDBM p = withDB p . runReaderT

liftDBM :: MonadDB c m => DBM a -> m a
liftDBM = liftDB . runReaderT

-- |Combination of 'liftDBM' and lifted 'tryJust'
dbTryJust :: MonadDB c m => (PGError -> Maybe e) -> DBM a -> m (Either e a)
dbTryJust err q = liftDB $ tryJust err . runReaderT q

dbRunQuery :: (MonadDB c m, PGQuery q a) => q -> m (Int, [a])
dbRunQuery q = liftDB $ \c -> pgRunQuery c q

dbExecute :: (MonadDB c m, PGQuery q ()) => q -> m Int
dbExecute q = liftDB $ \c -> pgExecute c q

dbExecuteSimple :: MonadDB c m => PGSimpleQuery () -> m Int
dbExecuteSimple = dbExecute

dbExecute1 :: (MonadDB c m, PGQuery q (), Show q) => q -> m Bool
dbExecute1 q = do
  r <- dbExecute q
  case r of
    0 -> return False
    1 -> return True
    _ -> fail $ "pgExecute1 " ++ show q ++ ": " ++ show r ++ " rows"

dbExecute1' :: (MonadDB c m, PGQuery q (), Show q) => q -> m ()
dbExecute1' q = do
  r <- dbExecute1 q
  unless r $ fail $ "pgExecute1' " ++ show q ++ ": failed"

dbExecute_ :: (MonadDB c m) => BSL.ByteString -> m ()
dbExecute_ q = liftDB $ \c -> pgSimpleQueries_ c q

dbQuery :: (MonadDB c m, PGQuery q a) => q -> m [a]
dbQuery q = liftDB $ \c -> pgQuery c q

dbQuery1 :: (MonadDB c m, PGQuery q a, Show q) => q -> m (Maybe a)
dbQuery1 q = do
  r <- dbQuery q
  case r of
    [] -> return $ Nothing
    [x] -> return $ Just x
    _ -> fail $ "pgQuery1 " ++ show q ++ ": too many results"

dbQuery1' :: (MonadDB c m, PGQuery q a, Show q) => q -> m a
dbQuery1' q = maybe (fail $ "pgQuery1' " ++ show q ++ ": no results") return =<< dbQuery1 q

dbTransaction :: MonadDB c m => DBM a -> m a
dbTransaction f = liftDB $ \c -> pgTransaction c (runReaderT f c)

dbTransaction' :: (MonadBaseControl IO m, MonadDB c m) => m a -> m a
dbTransaction' f = do
  c <- peek
  liftBaseOp_ (pgTransaction c) f

-- For connections outside runtime:

loadPGDatabase :: IO PGDatabase
loadPGDatabase = confPGDatabase . C.get "db" <$> C.load "databrary.conf"

runDBConnection :: DBM a -> IO a
runDBConnection f = bracket
  (pgConnect =<< loadPGDatabase)
  pgDisconnect
  (runReaderT f)

loadTDB :: TH.DecsQ
loadTDB = do 
  database <- TH.runIO loadPGDatabase 
  useTPGDatabase database

{-# NOINLINE usedTDB #-}
usedTDB :: IORef Bool
usedTDB = unsafePerformIO $ newIORef False
useTDB :: TH.DecsQ
useTDB = do
  d <- TH.runIO $ atomicModifyIORef' usedTDB ((,) True)
  if d
    then return []
    else loadTDB

runTDB :: DBM a -> TH.Q a
runTDB f = do
  _ <- useTDB
  TH.runIO $ withTPGConnection $ runReaderT f

-- Temporary helpers while removing postgresql-typed, remove after complete
mapQuery2 :: BS.ByteString -> ([PGValue] -> a) -> PGSimpleQuery a -- mapQuery is same as mapQuery2, both will be deleted
mapQuery2 qry mkResult =
  fmap mkResult (rawPGSimpleQuery qry)
