{-# LANGUAGE OverloadedStrings, ScopedTypeVariables, GeneralizedNewtypeDeriving
   , TypeSynonymInstances, MultiParamTypeClasses, FlexibleInstances, RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Databrary.Model.PartyTest where

-- import Control.Monad.IO.Class
import Control.Monad.Trans.Reader
-- import Control.Monad.Reader
import Data.Maybe
-- import Hedgehog
import Hedgehog.Gen as Gen
-- import Test.Tasty
import Test.Tasty.HUnit

-- import Databrary.Has
import Databrary.Model.Id
import Databrary.Model.Identity
import Databrary.Model.Party
import Databrary.Model.Party.TypesTest
-- import Databrary.Model.Permission
import Databrary.Service.DB
import TestHarness

unit_partyName_example :: Assertion
unit_partyName_example = do
    (partyName . partyRow) nobodyParty @?= "Everybody"

runLookupParty :: Id Party -> Identity -> Maybe Party -> Assertion
runLookupParty pid ident expected = do
    cn <- loadPGDatabase >>= pgConnect
    let ctxt = TestContext { ctxConn = cn, ctxIdentity = ident }
    --
    mParty <- runReaderT (lookupParty pid :: ReaderT TestContext IO (Maybe Party)) ctxt
    mParty @?= expected

unit_lookupParty :: Assertion
unit_lookupParty = do
    runLookupParty (Id 2) NotLoggedIn (Just staffParty)
    -- TODO: Fix these with real data parameters
    -- runLookupParty (Id 2) (Identified undefined) (Just staffParty)
    -- runLookupParty (Id 2) (ReIdentified undefined) (Just staffParty)

unit_lookupSiteAuthByEmail :: Assertion
unit_lookupSiteAuthByEmail = do
    cn <- loadPGDatabase >>= pgConnect
    let ctxt = TestContext { ctxConn = cn }
    mAuth <- runReaderT (lookupSiteAuthByEmail False "test@databrary.org") ctxt
    isJust mAuth @? "should find the well known test user's site auth by email"
    mAuth' <- runReaderT (lookupSiteAuthByEmail False "doesntexist@databrary.org") ctxt
    mAuth' @?= Nothing
    mAuth'' <- runReaderT (lookupSiteAuthByEmail True "TEST@DATABRARY.ORG") ctxt
    isJust mAuth'' @? "should find the well known test user's site auth by email, case insensitive"

unit_addAccount :: Assertion
unit_addAccount = withinTestTransaction (\cn -> do
    -- fill sortname, prename, email, affiliaton with blankParty
    -- fill email for account
    a <- fmap (fromJust . partyAccount) (Gen.sample genPartySimple)
    let ident = NotLoggedIn
        pid = Id (-1)
    a' <-
      runReaderT
        (addAccount a)
        (TestContext { ctxConn = cn, ctxIdentity = ident, ctxPartyId = pid, ctxRequest = defaultRequest })
    accountEmail a' @?=
      accountEmail a)

instance Eq Party where
    p1 == p2 =
           partyRow p1 == partyRow p2
        && partyPermission p1 == partyPermission p2
        && partyAccess p1 == partyAccess p2
        && equalAccounts (partyAccount p1) (partyAccount p2)

equalAccounts :: Maybe Account -> Maybe Account -> Bool
equalAccounts Nothing Nothing = True
equalAccounts (Just a1) (Just a2) =
    accountEmail a1 == accountEmail a2
equalAccounts _ _ = False

instance Eq SiteAuth where
    a1 == a2 =
           (accountParty . siteAccount) a1 == (accountParty . siteAccount) a2
        && accountPasswd a1 == accountPasswd a2
        && siteAccess a1 == siteAccess a2

instance Show Party where
  show p1 = show (partyRow p1, partyPermission p1, partyAccess p1, fmap accountEmail (partyAccount p1))

instance Show SiteAuth where
  show a = show (accountParty (siteAccount a), accountPasswd a, siteAccess a)
