module TestHarness
    (
      TestContext ( .. )
    , mkRequest
    , withStorage
    , withAV
    , withTimestamp
    , withLogs
    , withInternalStateVal
    , mkDbContext
    , runContextReaderT
    , withinTestTransaction
    , stepsWithTransaction
    , stepsWithResourceAndTransaction
    , connectTestDb
    , makeSuperAdminContext
    , fakeIdentSessFromAuth
    , addAuthorizedInstitution
    , addAuthorizedInvestigatorWithInstitution
    , addAuthorizedInvestigator
    , addAffiliate
    , lookupSiteAuthNoIdent
    , switchIdentity
    -- , addAuthorization
    , expect
    -- * re-export for convenience
    , runReaderT
    , Wai.defaultRequest
    , Id(..)
    , Identity(..)
    )
    where

import Control.Applicative
import Control.Exception (bracket)
import Control.Rematch
import Control.Rematch.Run
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Resource (InternalState, runResourceT, withInternalState)
import Data.Maybe
import Data.Time
import Database.PostgreSQL.Typed.Protocol
import qualified Hedgehog.Gen as Gen
import Test.Tasty
import Test.Tasty.HUnit
import qualified Data.ByteString as BS
-- import qualified Data.Text as T
import qualified Network.Wai as Wai
-- import qualified Network.Wai.Internal as Wai

import Databrary.Has
import Databrary.HTTP.Client
import Databrary.Model.Authorize
import Databrary.Model.Factories
import Databrary.Model.Id
import Databrary.Model.Identity
import Databrary.Model.Party
-- import Databrary.Model.Party.TypesTest
import Databrary.Model.Permission
import Databrary.Model.Time
import Databrary.Model.Token
import Databrary.Service.DB
import Databrary.Service.Entropy
import Databrary.Service.Log
import Databrary.Service.Types
import Databrary.Solr.Service (Solr)
import Databrary.Store.AV
import Databrary.Store.Config as C (load, (!))
import Databrary.Store.Service
-- import Databrary.Store.Types

-- Runtime dependencies
--   database started tests run
--   /databrary.conf has db credentials
--   database has seed data from 0.sql present
--   data file /test/data/small.webm present
--   TODO: desire to have passwd dict file in path
--   store directories present in /
--   /transcode, /transctl present
--   solr started using /solr-6.6.0 binaries w/core and config in /solr; databrary_logs created
--   ffmpeg exe on path
--   active internet connection for live http calls like geoname lookup to function

-- | Sloppily taken from hunit-rematch because author is too lazy
-- to update dependency bounds on hackage. Use fetch from his github later.
expect :: a -> Matcher a -> Assertion
expect a matcher = case res of
  MatchSuccess -> return ()
  (MatchFailure msg) -> assertFailure msg
  where res = runMatch matcher a

-- |
-- "God object" that can fulfill all needed "Has" instances. This is
-- intentionally quick to use for tests. The right way to use it is to keep all
-- fields undefined except those that the test in question is using: Since
-- runtime for tests is just as good as compile time for library, any bottoms
-- encountered will be a "good crash".
data TestContext = TestContext
    { ctxRequest :: Maybe Wai.Request
    -- ^ for MonadHasRequest
    , ctxSecret :: Maybe Secret
    , ctxEntropy :: Maybe Entropy
    -- ^ Both for MonadSign
    , ctxPartyId :: Maybe (Id Party)
    -- ^ for MonadAudit
    , ctxConn :: Maybe DBConn
    -- ^ for MonadDB
    , ctxStorage :: Maybe Storage
    -- ^ for MonadStorage
    , ctxSolr :: Maybe Solr
    -- ^ for MonadSolr
    , ctxInternalState :: Maybe InternalState
    , ctxIdentity :: Maybe Identity
    , ctxSiteAuth :: Maybe SiteAuth
    , ctxAV :: Maybe AV
    , ctxTimestamp :: Maybe Timestamp
    , ctxLogs :: Maybe Logs
    , ctxHttpClient :: Maybe HTTPClient
    }

blankContext :: TestContext
blankContext = TestContext
    { ctxRequest = Nothing
    , ctxSecret = Nothing
    , ctxEntropy = Nothing
    , ctxPartyId = Nothing
    , ctxConn = Nothing
    , ctxStorage = Nothing
    , ctxSolr = Nothing
    , ctxInternalState = Nothing
    , ctxIdentity = Nothing
    , ctxSiteAuth = Nothing
    , ctxAV = Nothing
    , ctxTimestamp = Nothing
    , ctxLogs = Nothing
    , ctxHttpClient = Nothing
    }

addCntxt :: TestContext -> TestContext -> TestContext
addCntxt c1 c2 =
    c1 {
          ctxRequest = ctxRequest c1 <|> ctxRequest c2
        , ctxSecret = ctxSecret c1 <|> ctxSecret c2
        , ctxEntropy = ctxEntropy c1 <|> ctxEntropy c2
        , ctxPartyId = ctxPartyId c1 <|> ctxPartyId c2
        , ctxConn = ctxConn c1 <|> ctxConn c2
        , ctxStorage = ctxStorage c1 <|> ctxStorage c2
        , ctxSolr = ctxSolr c1 <|> ctxSolr c2
        , ctxInternalState = ctxInternalState c1 <|> ctxInternalState c2
        , ctxIdentity = ctxIdentity c1 <|> ctxIdentity c2
        , ctxSiteAuth = ctxSiteAuth c1 <|> ctxSiteAuth c2
        , ctxAV = ctxAV c1 <|> ctxAV c2
        , ctxTimestamp = ctxTimestamp c1 <|> ctxTimestamp c2
        , ctxLogs = ctxLogs c1 <|> ctxLogs c2
        , ctxHttpClient = ctxHttpClient c1 <|> ctxHttpClient c2
       }

instance Has Identity TestContext where
    view = fromJust . ctxIdentity

instance Has DBConn TestContext where
    view = fromJust . ctxConn

instance Has Wai.Request TestContext where
    view = fromJust . ctxRequest

instance Has Secret TestContext where
    view = fromJust . ctxSecret

instance Has Entropy TestContext where
    view = fromJust . ctxEntropy

instance Has AV TestContext where
    view = fromJust . ctxAV

instance Has Solr TestContext where
    view = fromJust . ctxSolr

instance Has Storage TestContext where
    view = fromJust . ctxStorage

instance Has InternalState TestContext where
    view = fromJust . ctxInternalState

instance Has Timestamp TestContext where
    view = fromJust . ctxTimestamp

instance Has Logs TestContext where
    view = fromJust . ctxLogs

instance Has HTTPClient TestContext where
    view = fromJust . ctxHttpClient

-- Needed for types, but unused so far

-- prefer using SiteAuth instead of Identity for test contexts
instance Has SiteAuth TestContext where
    view = fromJust . ctxSiteAuth

instance Has Party TestContext where
    view = undefined

instance Has (Id Party) TestContext where
    view = fromJust . ctxPartyId

instance Has Access TestContext where
    view = view . fromJust . ctxIdentity

mkRequest :: Wai.Request
mkRequest = Wai.defaultRequest { Wai.requestHeaderHost = Just "invaliddomain.org" }

withStorage :: TestContext -> IO TestContext
withStorage ctxt = do
    addCntxt ctxt <$> mkStorageContext

mkStorageContext :: IO TestContext
mkStorageContext = do
    conf <- load "databrary.conf"
    stor <- initStorage (conf C.! "store")
    pure (blankContext { ctxStorage = Just stor })

withAV :: TestContext -> IO TestContext
withAV ctxt = do
    addCntxt ctxt <$> mkAVContext

withTimestamp :: Timestamp -> TestContext -> TestContext
withTimestamp ts ctxt =
    addCntxt ctxt (blankContext { ctxTimestamp = Just ts })

withLogs :: TestContext -> IO TestContext
withLogs ctxt = do
    logs <- initLogs mempty
    pure (addCntxt ctxt (blankContext { ctxLogs = Just logs }))

mkAVContext :: IO TestContext
mkAVContext = do
    av <- initAV
    pure (blankContext { ctxAV = Just av })

withInternalStateVal :: InternalState -> TestContext -> TestContext
withInternalStateVal ist ctxt =
    addCntxt ctxt (blankContext { ctxInternalState = Just ist })

-- | Convenience for building a context with only a db connection
mkDbContext :: DBConn -> TestContext
mkDbContext c = blankContext { ctxConn = Just c }

-- | Convenience for runReaderT where context consists of db connection only
runContextReaderT :: DBConn -> ReaderT TestContext IO a -> IO a
runContextReaderT cn rdrActions = runReaderT rdrActions (blankContext { ctxConn = Just cn })

-- | Run an action that uses a db connection and also needs to use internal state/resourcet.
-- TODO: make this cleaner.
withinResourceAndTransaction :: (InternalState -> PGConnection -> IO a) -> IO a
withinResourceAndTransaction act =
    runResourceT $ withInternalState $ \ist ->
        withinTestTransaction (act ist)

-- | Execute a test within a DB connection that rolls back at the end.
withinTestTransaction :: (PGConnection -> IO a) -> IO a
withinTestTransaction act =
     bracket
         (do
              cn <- pgConnect =<< loadPGDatabase
              pgBegin cn
              pure cn)
         (\cn -> pgRollback cn >> pgDisconnect cn)
         act

-- | Combine 'testCaseSteps' and 'withinResourceandTransaction'
stepsWithResourceAndTransaction
    :: TestName -> ((String -> IO ()) -> InternalState -> PGConnection -> IO ()) -> TestTree
stepsWithResourceAndTransaction name f =
    testCaseSteps name (\step -> withinResourceAndTransaction (f step))

-- | Combine 'testCaseSteps' and 'withinTestTransaction'
stepsWithTransaction
    :: TestName -> ((String -> IO ()) -> PGConnection -> IO ()) -> TestTree
stepsWithTransaction name f =
    testCaseSteps name (\step -> withinTestTransaction (f step))

connectTestDb :: IO PGConnection
connectTestDb =
    loadPGDatabase >>= pgConnect

makeSuperAdminContext :: PGConnection -> BS.ByteString -> IO TestContext -- login + spawn context
makeSuperAdminContext cn adminEmail =
    runReaderT
        (do
             Just auth <- lookupSiteAuthByEmail False adminEmail
             let pid = (partyId . partyRow . accountParty . siteAccount) auth
                 ident = fakeIdentSessFromAuth auth True
             pure (blankContext {
                        ctxConn = Just cn
                      , ctxIdentity = Just ident
                      , ctxSiteAuth = Just (view ident)
                      , ctxPartyId = Just pid
                      , ctxRequest = Just Wai.defaultRequest
                      }))
        blankContext { ctxConn = Just cn }

fakeIdentSessFromAuth :: SiteAuth -> Bool -> Identity
fakeIdentSessFromAuth a su =
    Identified
      (Session
         (AccountToken (Token (Id "id") (UTCTime (fromGregorian 2017 1 2) (secondsToDiffTime 0))) a)
         "verf"
         su)

addAuthorizedInstitution :: TestContext -> IO Party  -- create + approve as site admin
addAuthorizedInstitution adminCtxt = do
    createInst <- Gen.sample genCreateInstitutionParty
    runReaderT
        (do
             created <- addParty createInst
             changeAuthorize (makeAuthorize (Access PermissionADMIN PermissionNONE) Nothing created rootParty)
             pure created)
        adminCtxt

-- TODO: recieve expiration date (expiration dates might not be used...)  -- register as anon + approve as site admin
addAuthorizedInvestigator :: TestContext -> Party -> IO Account
addAuthorizedInvestigator adminCtxt instParty = do
    let ctxtNoIdent =
          adminCtxt { ctxIdentity = Just IdentityNotNeeded, ctxPartyId = Just (Id (-1)), ctxSiteAuth = Just (view IdentityNotNeeded) }
    a <- Gen.sample genAccountSimple
    aiAccount <-
        runReaderT
            (do
                 created <- addAccount a
                 Just auth <- lookupSiteAuthByEmail False (accountEmail a)
                 changeAccount (auth { accountPasswd = Just "somehashedvalue" })
                 pure created)
            ctxtNoIdent
    runReaderT
        (changeAuthorize (makeAuthorize (Access PermissionADMIN PermissionNONE) Nothing (accountParty aiAccount) instParty))
        adminCtxt
    pure aiAccount

-- registerConfirm :: DBConn -> IO Account
-- registerConfirm cn = do
    -- some account w/email
    -- runWithoutIdent addAccount + lookupAuth + changeAccount

addAuthorizedInvestigatorWithInstitution :: DBConn -> BS.ByteString -> IO (Account, TestContext)
addAuthorizedInvestigatorWithInstitution cn adminEmail = do
    ctxt <- makeSuperAdminContext cn adminEmail
    instParty <- addAuthorizedInstitution ctxt
    aiAcct <- addAuthorizedInvestigator ctxt instParty

    -- login as AI, bld cntxt
    let ctxtNoIdent = ctxt { ctxIdentity = Just IdentityNotNeeded, ctxPartyId = Just (Id (-1)), ctxSiteAuth = Just (view IdentityNotNeeded) }
    Just aiAuth <- runReaderT (lookupSiteAuthByEmail False (accountEmail aiAcct)) ctxtNoIdent
    let aiCtxt = switchIdentity ctxt aiAuth False
    pure (aiAcct, aiCtxt)

-- TODO: receive expiration date    -- register as anon + approve as ai
addAffiliate :: TestContext -> Party -> Permission -> Permission -> IO Account
addAffiliate aiCntxt aiParty site member = do
    let ctxtNoIdent =
          aiCntxt { ctxIdentity = Just IdentityNotNeeded, ctxPartyId = Just (Id (-1)), ctxSiteAuth = Just (view IdentityNotNeeded) }
    a <- Gen.sample genAccountSimple
    affAccount <-
        runReaderT
            (do
                 created <- addAccount a
                 Just auth <- lookupSiteAuthByEmail False (accountEmail a)
                 changeAccount (auth { accountPasswd = Just "somehashedvalue" })
                 pure created)
            ctxtNoIdent
    runReaderT
        (changeAuthorize (makeAuthorize (Access site member) Nothing (accountParty affAccount) aiParty))
        aiCntxt
    pure affAccount

{-
-- TODO: receive authorization
addAuthorization :: TestContext -> Party -> Party -> Permission -> Permission -> IO ()
addAuthorization ctxt parentParty requestParty site member = do
    runReaderT
        (changeAuthorize (makeAuthorize (Access site member) Nothing requestParty parentParty))
        ctxt
-}

lookupSiteAuthNoIdent :: TestContext -> BS.ByteString -> IO SiteAuth
lookupSiteAuthNoIdent privCtxt email = do
    let ctxtNoIdent =
          privCtxt { ctxIdentity = Just IdentityNotNeeded, ctxPartyId = Just (Id (-1)), ctxSiteAuth = Just (view IdentityNotNeeded) }
    fromJust <$> runReaderT (lookupSiteAuthByEmail False email) ctxtNoIdent

switchIdentity :: TestContext -> SiteAuth -> Bool -> TestContext
switchIdentity baseCtxt auth su = do
    baseCtxt {
          ctxIdentity = Just (fakeIdentSessFromAuth auth su)
        , ctxPartyId = Just ((partyId . partyRow . accountParty . siteAccount) auth)
        , ctxSiteAuth = Just auth
    }
