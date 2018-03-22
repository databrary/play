{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module Databrary.Model.AuditTest where

-- import qualified Data.HashMap.Strict as HMP
-- import qualified Data.Vector as V
-- import Database.PostgreSQL.Typed.Inet (PGInet(..), sockAddrPGInet)
import Test.Tasty
import Test.Tasty.HUnit

-- import Databrary.Has
import Databrary.Model.Audit
-- import Databrary.HTTP.Request

tests :: TestTree
tests = testGroup "Databrary.Model.AuditTest"
    [
{-
    [ testCase "getRemoteIp-1" (do
          ipVal <- getRemoteIp
          ipVal @?= undefined)
-}
    ]
