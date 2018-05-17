module TestHarness
    (
      TestContext ( .. )
    , withinTestTransaction
    , stepsWithTransaction
    , connectTestDb
    , makeSuperAdminContext
    , fakeIdentSessFromAuth
    , addAuthorizedInstitution
    , mkInstitution -- TODO: stop exporting
    , mkAccount -- TODO: stop exporting
    , addAuthorizedInvestigator
    , addAffiliate
    , lookupSiteAuthNoIdent
    , switchIdentity
    -- , addAuthorization
    -- * re-export for convenience
    , runReaderT
    , Wai.defaultRequest
    , Id(..)
    , Identity(..)
    , module Databrary.Model.Permission
    )
    where

import Control.Exception (bracket)
import Control.Monad.Trans.Reader
import Data.Maybe
import Data.Time
import Database.PostgreSQL.Typed.Protocol
import Test.Tasty
import Test.Tasty.HUnit
import qualified Data.ByteString as BS
import qualified Data.Text as T
import qualified Network.Wai as Wai

import Databrary.Has
import Databrary.Model.Authorize
import Databrary.Model.Id
import Databrary.Model.Identity
import Databrary.Model.Party
import Databrary.Model.Permission
import Databrary.Model.Token
import Databrary.Service.DB
import Databrary.Service.Entropy
import Databrary.Service.Types
import Databrary.Store.AV


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

makeSuperAdminContext :: PGConnection -> BS.ByteString -> IO TestContext
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

addAuthorizedInstitution :: TestContext -> T.Text -> IO Party
addAuthorizedInstitution adminCtxt instName = do
    runReaderT
        (do
             created <- addParty (mkInstitution instName)
             changeAuthorize (makeAuthorize (Access PermissionADMIN PermissionNONE) Nothing created rootParty)
             pure created)
        adminCtxt

-- TODO: recieve expiration date
addAuthorizedInvestigator :: TestContext -> T.Text -> T.Text -> BS.ByteString -> Party -> IO Account
addAuthorizedInvestigator adminCtxt lastName firstName email instParty = do
    let ctxtNoIdent = adminCtxt { ctxIdentity = IdentityNotNeeded, ctxPartyId = Id (-1), ctxSiteAuth = view IdentityNotNeeded }
        a = mkAccount lastName firstName email
    aiAccount <-
        runReaderT
            (do
                 created <- addAccount a
                 Just auth <- lookupSiteAuthByEmail False email
                 changeAccount (auth { accountPasswd = Just "somehashedvalue" })
                 pure created)
            ctxtNoIdent
    runReaderT
        (changeAuthorize (makeAuthorize (Access PermissionADMIN PermissionNONE) Nothing (accountParty aiAccount) instParty))
        adminCtxt
    pure aiAccount

-- TODO: receive expiration date
addAffiliate :: TestContext -> T.Text -> T.Text -> BS.ByteString -> Party -> Permission -> Permission -> IO Account
addAffiliate aiCntxt lastName firstName email aiParty site member = do
    let ctxtNoIdent = aiCntxt { ctxIdentity = IdentityNotNeeded, ctxPartyId = Id (-1), ctxSiteAuth = view IdentityNotNeeded }
        a = mkAccount lastName firstName email
    affAccount <-
        runReaderT
            (do
                 created <- addAccount a
                 Just auth <- lookupSiteAuthByEmail False email
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

mkInstitution :: T.Text -> Party
mkInstitution instName =
    blankParty {
          partyRow = (partyRow blankParty) { partySortName = instName }
        }

mkAccount :: T.Text -> T.Text -> BS.ByteString -> Account
mkAccount sortName preName email = 
    let pr = (partyRow blankParty) { partySortName = sortName , partyPreName = Just preName }
        p = blankParty { partyRow = pr, partyAccount = Just a }
        a = blankAccount { accountParty = p, accountEmail = email }
    in a

switchIdentity :: TestContext -> SiteAuth -> Bool -> TestContext
switchIdentity baseCtxt auth su = do
    baseCtxt {
          ctxIdentity = fakeIdentSessFromAuth auth su
        , ctxPartyId = (partyId . partyRow . accountParty . siteAccount) auth
        , ctxSiteAuth = auth
    }
