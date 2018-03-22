{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module Databrary.Model.Audit.TypesTest where

import Database.PostgreSQL.Typed.Inet (PGInet(..))
import Test.Tasty
import Test.Tasty.HUnit

import Databrary.Model.Audit.Types
import Databrary.Model.Id.Types

auditIdentity1 :: AuditIdentity
auditIdentity1 =
    AuditIdentity {
          auditWho = Id 2
        , auditIp = PGInet 32 0
        }

tests :: TestTree
tests = testGroup "Databrary.Model.Audit.Types"
    [
    ]
