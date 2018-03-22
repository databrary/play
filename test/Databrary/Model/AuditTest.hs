{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module Databrary.Model.AuditTest where

import Control.Monad.Trans.Reader (ReaderT(..), withReaderT)
-- import qualified Data.HashMap.Strict as HMP
-- import qualified Data.Vector as V
import Data.Functor.Identity
import Database.PostgreSQL.Typed.Inet (PGInet(..), sockAddrPGInet)
import qualified Network.Wai as Wai
import Test.Tasty
import Test.Tasty.HUnit

-- import Databrary.Has
import Databrary.Model.Audit
-- import Databrary.HTTP.Request

-- type MonadHas a c m = (Functor m, Applicative m, MonadReader c m, Has a c)
-- type MonadHasRequest c m = MonadHas Wai.Request Wai.Request Identity
-- ReaderT Wai.Request Identity a

-- withReaderT (req) :: ReaderT Wai.Request Identity a
-- runReaderT rdr req :: Identity a

tests :: TestTree
tests = testGroup "Databrary.Model.AuditTest"
    [ testCase "getRemoteIp-1" (
          let req :: Wai.Request
              req = undefined
              identVal = (runReaderT getRemoteIp req :: Identity PGInet)
          in
              True @?= True)
    ]
