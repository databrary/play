module TestHarness
    (
      TestContext ( .. )
    -- * re-export for convenience
    , runReaderT
    )
    where

import Control.Monad.Trans.Reader
import qualified Network.Wai as Wai

import Databrary.Has
import Databrary.Model.Id
import Databrary.Model.Identity
import Databrary.Model.Party
import Databrary.Model.Permission
import Databrary.Service.DB
import Databrary.Service.Entropy
import Databrary.Service.Types


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
    , ctxConn :: DBConn
    , ctxIdentity :: Identity
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

instance Has SiteAuth TestContext where
    view = undefined

instance Has Party TestContext where
    view = undefined

instance Has (Id Party) TestContext where
    view = undefined

instance Has Access TestContext where
    view = undefined
