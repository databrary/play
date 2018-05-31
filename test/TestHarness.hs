module TestHarness
    (
      TestContext ( .. )
    , mkDbContext
    , runContextReaderT
    , withinTestTransaction
    , stepsWithTransaction
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

import Control.Exception (bracket)
import Control.Rematch
import Control.Rematch.Run
import Control.Monad.Trans.Reader
import Data.Maybe
import Data.Time
import Database.PostgreSQL.Typed.Protocol
import qualified Hedgehog.Gen as Gen
import Test.Tasty
import Test.Tasty.HUnit
import qualified Data.ByteString as BS
-- import qualified Data.Text as T
import qualified Network.Wai as Wai

import Databrary.Has
import Databrary.Model.Authorize
import Databrary.Model.Id
import Databrary.Model.Identity
import Databrary.Model.Party
import Databrary.Model.Party.TypesTest
import Databrary.Model.Permission
import Databrary.Model.Token
import Databrary.Service.DB
import Databrary.Service.Entropy
import Databrary.Service.Types
import Databrary.Store.AV

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
    { ctxRequest :: Wai.Request
    -- ^ for MonadHasRequest
    , ctxSecret :: Secret
    , ctxEntropy :: Entropy
    -- ^ Both for MonadSign
    , ctxPartyId :: Id Party
    -- ^ for MonadAudit
    , ctxConn :: DBConn
    -- ^ for MonadDB
    , ctxIdentity :: Identity
    , ctxSiteAuth :: SiteAuth
    , ctxAV :: AV
    }

instance Has Identity TestContext where
    view = ctxIdentity

instance Has DBConn TestContext where
    view = ctxConn

instance Has Wai.Request TestContext where
    view = ctxRequest

instance Has Secret TestContext where
    view = ctxSecret

instance Has Entropy TestContext where
    view = ctxEntropy

-- Needed for types, but unused so far

-- prefer using SiteAuth instead of Identity for test contexts
instance Has SiteAuth TestContext where
    view = ctxSiteAuth

instance Has Party TestContext where
    view = undefined

instance Has (Id Party) TestContext where
    view = ctxPartyId

instance Has Access TestContext where
    view = view . ctxIdentity

instance Has AV TestContext where
    view = ctxAV

-- | Convenience for building a context with only a db connection
mkDbContext :: DBConn -> TestContext
mkDbContext c = TestContext { ctxConn = c }

-- | Convenience for runReaderT where context consists of db connection only
runContextReaderT :: DBConn -> ReaderT TestContext IO a -> IO a
runContextReaderT cn rdrActions = runReaderT rdrActions (TestContext { ctxConn = cn })

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
             pure (TestContext {
                        ctxConn = cn
                      , ctxIdentity = ident
                      , ctxSiteAuth = view ident
                      , ctxPartyId = pid
                      , ctxRequest = Wai.defaultRequest
                      }))
        TestContext { ctxConn = cn }

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

-- TODO: recieve expiration date  -- register as anon + approve as site admin
addAuthorizedInvestigator :: TestContext -> Party -> IO Account
addAuthorizedInvestigator adminCtxt instParty = do
    let ctxtNoIdent = adminCtxt { ctxIdentity = IdentityNotNeeded, ctxPartyId = Id (-1), ctxSiteAuth = view IdentityNotNeeded }
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

    let ctxtNoIdent = ctxt { ctxIdentity = IdentityNotNeeded, ctxPartyId = Id (-1), ctxSiteAuth = view IdentityNotNeeded } -- login as AI, bld cntxt
    Just aiAuth <- runReaderT (lookupSiteAuthByEmail False (accountEmail aiAcct)) ctxtNoIdent
    let aiCtxt = switchIdentity ctxt aiAuth False
    pure (aiAcct, aiCtxt)

-- TODO: receive expiration date    -- register as anon + approve as ai
addAffiliate :: TestContext -> Party -> Permission -> Permission -> IO Account
addAffiliate aiCntxt aiParty site member = do
    let ctxtNoIdent = aiCntxt { ctxIdentity = IdentityNotNeeded, ctxPartyId = Id (-1), ctxSiteAuth = view IdentityNotNeeded }
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
    let ctxtNoIdent = privCtxt { ctxIdentity = IdentityNotNeeded, ctxPartyId = Id (-1), ctxSiteAuth = view IdentityNotNeeded }
    fromJust <$> runReaderT (lookupSiteAuthByEmail False email) ctxtNoIdent

switchIdentity :: TestContext -> SiteAuth -> Bool -> TestContext
switchIdentity baseCtxt auth su = do
    baseCtxt {
          ctxIdentity = fakeIdentSessFromAuth auth su
        , ctxPartyId = (partyId . partyRow . accountParty . siteAccount) auth
        , ctxSiteAuth = auth
    }
