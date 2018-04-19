{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module Databrary.Model.AuditTest where

import Control.Monad.Trans.Reader (ReaderT(..))
-- import qualified Data.HashMap.Strict as HMP
-- import qualified Data.Vector as V
import Data.Functor.Identity
import Database.PostgreSQL.Typed.Inet (PGInet(..))
import qualified Network.Wai as Wai
import Test.Tasty
import Test.Tasty.HUnit

import Databrary.Model.Audit

test_all :: [TestTree]
test_all =
    [ testCase "getRemoteIp-1" (
          let req :: Wai.Request
              req = Wai.defaultRequest
              inetAddr = (runIdentity (runReaderT getRemoteIp req :: Identity PGInet))
          in
              inetAddr @?= PGInet 0 32)
    ]
