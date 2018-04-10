{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module Databrary.Model.Audit.TypesTest where

import Data.Time as Time
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

audit1 :: Audit
audit1 =
    Audit {
          auditWhen = Time.UTCTime (Time.fromGregorian 2000 1 2) (Time.secondsToDiffTime 0)
        , auditIdentity = auditIdentity1
        , auditAction = AuditActionOpen
        }

test_all :: TestTree
test_all = testGroup "all"
    [
    ]
