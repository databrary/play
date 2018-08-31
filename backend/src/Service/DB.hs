{-# LANGUAGE FlexibleInstances, FlexibleContexts,
  MultiParamTypeClasses, ConstraintKinds, TypeFamilies,
  OverloadedStrings #-}
module Service.DB
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
  , mapPrepQuery
  , mapRunPrepQuery
  , mapRunPrepQuery1
  -- FIXME: added for tests
  , loadPGDatabase
  , pgConnect
  -- * DB configuration lookup
  --
  -- $getDBDoc
  , getDBUser
  , getDBDatabase
  , getDBHost
  ) where

import Control.Applicative
import Control.Exception (tryJust, bracket)
import Control.Monad (unless)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Trans.Control (MonadBaseControl, liftBaseOp_)
import Control.Monad.Trans.Reader (ReaderT(..))
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Char8 as BS
import Data.IORef (IORef, newIORef, atomicModifyIORef')
import Data.Maybe (fromMaybe, isJust)
import Data.Pool (Pool, withResource, createPool, destroyAllResources)
import qualified Database.PostgreSQL.Simple as PGSimple
import Database.PostgreSQL.Typed.Protocol
import Database.PostgreSQL.Typed.Query
import Database.PostgreSQL.Typed.TH (withTPGConnection, useTPGDatabase)
import Database.PostgreSQL.Typed.Types (PGValue)
import qualified Language.Haskell.TH as TH
import Network (PortID(..), HostName)
import System.Directory (doesFileExist, doesDirectoryExist)
import System.IO.Unsafe (unsafePerformIO)
import System.Environment (lookupEnv)
import qualified System.FilePath as FilePath
import qualified Text.Read.Located

import Has
import qualified Store.Config as C

confPGDatabase :: C.Config -> BS.ByteString -> HostName -> PortID -> PGDatabase
confPGDatabase conf user host port = defaultPGDatabase
  { pgDBHost = host
  , pgDBPort = port
  , pgDBName = fromMaybe user $ conf C.! "db"
  , pgDBUser = user
  , pgDBPass = fromMaybe "" $ conf C.! "pass"
  , pgDBDebug = fromMaybe False $ conf C.! "debug"
  }

-- |
-- $getDBDoc
--
-- postgresql-typed uses its own env vars, separate from those used by Postgres.
-- This pains me. The getDBFoo functions create a connection configuration using
-- the original vars when possible, falling back to Databrary's original legacy
-- config file if necessary.
--
--

-- | Uses PGUSER and/or USER, falls back to \"user\".
getDBUser :: C.Config -> IO BS.ByteString
getDBUser conf = do
    pguser <- fmap BS.pack <$> lookupEnv "PGUSER"
    user <- fmap BS.pack <$> lookupEnv "USER"
    let confUser = conf C.! "user"
    pure (fromMaybe confUser (pguser <|> user))

-- | Uses PGDATABASE, falls back to \"name\".
getDBDatabase :: C.Config -> IO BS.ByteString
getDBDatabase conf = do
    pgdb <- fmap BS.pack <$> lookupEnv "PGDATABASE"
    let confdb = conf C.! "db"
    pure (fromMaybe confdb pgdb)

-- | Postgres is smart enough to choose a TCP connection or Unix socket based on
-- the format of PGHOST. postgresql-typed, however, uses a combination of host
-- and "port", and the latter can be a port or a socket. This follows the
-- deprecated Network.PortID type, which doesn't make any sense anyway.
--
-- I will duplicate Postgres' logic and use a combination of PGHOST and PGPORT,
-- falling back to \"host\" and \"port\"/\"sock\" using legacy Databrary logic
-- (further falling back to \"localhost\" and port 5432.)
--
-- To further constrain the possibilities, PGHOST will be a sentinel. If PGHOST
-- is defined, the conf file will be skipped entirely. If PGHOST is *not*
-- defined, PGPORT will be skipped entirely.
getDBHost :: C.Config -> IO (HostName, PortID)
getDBHost conf = do
    menvHost <- lookupEnv "PGHOST"
    envHostIsDir <- maybe (pure False) doesDirectoryExist menvHost
    envPort <- fromMaybe "5432" <$> lookupEnv "PGPORT"
    let mconfHost = conf C.! "host"
        confPort =
            -- Legacy handling of "port" versus "sock".
            if isJust mconfHost
            then fromMaybe "5432" (conf C.! "port")
            else conf C.! "sock"
    confPortIsFile <- doesFileExist confPort
    pure
        (calculateDBHost
            menvHost
            envHostIsDir
            envPort
            mconfHost
            confPort
            confPortIsFile
        )

-- | Pure meat of getDBHost for unit testing
--
-- >>> calculateDBHost (Just "x") "5432" (Just "ignored") (Just "ignored")
-- ("x", PortNumber 5432)
--
-- >>> calculateDBHost (Just ".") "5444" (Just "ignored") (Just "ignored")
-- ("localhost", UnixSocket "./.s.PGSQL.5444")
--
calculateDBHost
    :: Maybe String
    -- ^ host from env
    -> Bool
    -- ^ does the former arg correspond to a local directory?
    -> String
    -- ^ port from env (or fallback of \"5432\")
    -> Maybe String
    -- ^ host from conf file
    -> String
    -- ^ port from conf file (or fallback of \"5432\")
    -> Bool
    -- ^ does the former arg correspond to a local file?
    -> (HostName, PortID)
calculateDBHost menvHost envHostIsDir envPort mconfHost confPort confPortIsFile =
    case menvHost of
        Just envHost
            | envHostIsDir ->
                ( "localhost"
                , UnixSocket (envHost FilePath.</> ".s.PGSQL." ++ envPort))
            | otherwise ->
                (envHost, PortNumber (fromIntegral (Text.Read.Located.read envPort)))
        Nothing -> -- No env host
            case mconfHost of
                Just confHost ->
                    (confHost, PortNumber (fromIntegral (Text.Read.Located.read confPort)))
                Nothing
                    | confPortIsFile ->
                        ("localhost", UnixSocket confPort)
                    | otherwise ->
                        ("localhost", PortNumber (fromIntegral (Text.Read.Located.read confPort)))

data DBPool = DBPool (Pool PGConnection) (Pool PGSimple.Connection)
type DBConn = PGConnection

initDB :: C.Config -> IO DBPool
initDB conf = do
    (host, port) <- getDBHost conf
    user <- getDBUser conf
    let db = confPGDatabase conf user host port
    DBPool
        <$> createPool' (pgConnect db) pgDisconnect
        <*> createPool' (PGSimple.connect (simpleConnInfo db)) (PGSimple.close)
  where
    createPool' :: IO a -> (a -> IO ()) -> IO (Pool a)
    createPool' get release =
        createPool get release stripes (fromInteger idle) conn
    simpleConnInfo db = PGSimple.defaultConnectInfo
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
    [] -> return Nothing
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
loadPGDatabase = do
    dbConf <- C.get "db" <$> C.load "databrary.conf"
    user <- getDBUser dbConf
    (host, port) <- getDBHost dbConf
    pure (confPGDatabase dbConf user host port)

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

mapPrepQuery :: (BS.ByteString, [PGValue]) -> ([PGValue] -> a) -> PGPreparedQuery a
mapPrepQuery (qry, params) mkResult =
  fmap mkResult (rawPGPreparedQuery qry params)

mapRunPrepQuery :: (MonadDB c m) => (BS.ByteString, [PGValue], [Bool]) -> ([PGValue] -> a) -> m [a]
mapRunPrepQuery (qry, params, bc) mkResult = do
  rows <- liftDB $ \c -> snd <$> pgPreparedQuery c qry [] params bc
  pure (fmap mkResult rows)

mapRunPrepQuery1 :: (MonadDB c m) => (BS.ByteString, [PGValue], [Bool]) -> ([PGValue] -> a) -> m (Maybe a)
mapRunPrepQuery1 args@(q, _, _) mkResult = do
  rows <- mapRunPrepQuery args mkResult
  case rows of
    [] -> return Nothing
    [x] -> return $ Just x
    _ -> fail $ "pgQuery1 " ++ show q ++ ": too many results"
