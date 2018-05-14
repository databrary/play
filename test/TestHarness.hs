module TestHarness
    (
      TestContext ( .. )
    , withinTestTransaction
    , connectTestDb
    -- * re-export for convenience
    , runReaderT
    , Wai.defaultRequest
    , Id(..)
    , Identity(..)
    )
    where

import Control.Exception (bracket)
import Control.Monad.Trans.Reader
import Database.PostgreSQL.Typed.Protocol
import qualified Network.Wai as Wai

import Databrary.Has
import Databrary.Model.Id
import Databrary.Model.Identity
import Databrary.Model.Party
import Databrary.Model.Permission
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

withinTestTransaction :: (PGConnection -> IO a) -> IO a
withinTestTransaction act =
     bracket
         (do
              cn <- pgConnect =<< loadPGDatabase
              pgBegin cn
              pure cn)
         (\cn -> pgRollback cn >> pgDisconnect cn)
         act

connectTestDb :: IO PGConnection
connectTestDb =
    loadPGDatabase >>= pgConnect
