{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module Databrary.Model.AuthorizeTest where

import Test.Tasty
import Test.Tasty.HUnit

import Databrary.Model.Authorize
import Databrary.Model.Party
import Databrary.Model.Permission
import TestHarness

-- session exercise various logic in Authorize
test_Authorize_examples :: TestTree
test_Authorize_examples = testCaseSteps "Authorize examples" $ \step -> do
  (authorizeExpires . selfAuthorize) nobodyParty @?= Nothing

  cn <- connectTestDb
  step "Given an admin user"
  let adminUser = nobodyParty { partyRow = (partyRow nobodyParty) { partyId = Id 7 } }
  step "When we look at its direct authorization on databrary site"
  Just auth <- runReaderT (lookupAuthorize adminUser rootParty) TestContext { ctxConn = cn }
  step "Then we expect the authorization to have site and member level of ADMIN"
  (authorizeAccess . authorization) auth @?= Access { accessSite' = PermissionADMIN, accessMember' = PermissionADMIN }

  -- create super user account (identity not needed)
  -- save authorize as ADMIN on databrary (identity not needed)
  -- lookup super user site auth

  -- create institution party (super admin ident)
  -- save authorize as EDIT on databrary (super admin ident)
  -- lookup institution party

  -- create institution party (super admin ident)
  -- save authorize as EDIT on databrary (super admin ident)
  -- create AI account (identity not needed)
  -- update AI account (identity not needed)
  -- save authorize as EDIT on instituion (super admin ident or admin ident?)
  -- lookup AI account

  --- ... w/AI + affiliate; lookup affiliate account

  -- repeat scenarios above but perform a restricted action (create volume as AI, view volume as affiliate)
  
