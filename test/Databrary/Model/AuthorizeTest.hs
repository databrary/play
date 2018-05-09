{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module Databrary.Model.AuthorizeTest where

import Test.Tasty.HUnit

import Databrary.Model.Authorize
import Databrary.Model.Party

-- session exercise various logic in Authorize
unit_Authorize_examples :: Assertion
unit_Authorize_examples = do
  (authorizeExpires . selfAuthorize) nobodyParty @?= Nothing
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
  
