{-# LANGUAGE OverloadedStrings, ScopedTypeVariables, GeneralizedNewtypeDeriving
   , TypeSynonymInstances, MultiParamTypeClasses, FlexibleInstances, RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Databrary.Model.PartyTest where

-- import Control.Monad.IO.Class
import Control.Monad.Trans.Reader
-- import Control.Monad.Reader
-- import qualified Data.ByteString as BS
import Data.Maybe
-- import qualified Data.Text as T
-- import Data.Time
-- import Hedgehog
import Hedgehog.Gen as Gen
-- import Test.Tasty
import Test.Tasty.HUnit

-- import Databrary.Has
import Databrary.Model.Id
import Databrary.Model.Identity
import Databrary.Model.Party
import Databrary.Model.Party.TypesTest
import Databrary.Model.Permission
-- import Databrary.Model.Token
import Databrary.Service.DB
import TestHarness

-- session driving a variety of functions in party module
unit_Party_examples :: Assertion
unit_Party_examples = do
    -- example of core logic within register + set password + login.
    -- most likely same logic for creating AI or Affiliate.
    withinTestTransaction (\cn -> do
        let a = mkAccount "smith" "john" "john@smith.com"
        let ident = IdentityNotNeeded
            pid = Id (-1)
            ctxt = TestContext { ctxConn = cn, ctxIdentity = ident, ctxPartyId = pid, ctxRequest = defaultRequest }
        Just auth2 <-
            runReaderT
              (do
                  _ <- addAccount a
                  Just auth <- lookupSiteAuthByEmail False "john@smith.com"
                  changeAccount auth { accountPasswd = Just "somehashedvalue" }
                  lookupSiteAuthByEmail False "john@smith.com")
              ctxt
        let p = (accountParty . siteAccount) auth2
        (accountEmail . siteAccount) auth2 @?= "john@smith.com"
        accountPasswd auth2 @?= Just "somehashedvalue"
        siteAccess auth2 @?= Access { accessSite' = PermissionNONE, accessMember' = PermissionNONE }
        -- TODO: explain the values below
        partyPermission p @?= PermissionADMIN
        partyAccess p @?= Just (Access { accessSite' = PermissionADMIN, accessMember' = PermissionADMIN }))

    -- example of core logic within login as superadmin + create instituion party + view created party
    withinTestTransaction (\cn -> do
        ctxt <-
            runReaderT
                (do
                     Just auth <- lookupSiteAuthByEmail False "test@databrary.org"
                     let pid = Id 7
                         ident = fakeIdentSessFromAuth auth True
                     pure (TestContext { ctxConn = cn, ctxIdentity = ident, ctxPartyId = pid, ctxRequest = defaultRequest }))
                TestContext { ctxConn = cn }
        p <- Gen.sample genCreateInstitutionParty
        Just p' <-
            runReaderT
              (do
                  created <- addParty p
                  lookupParty ((partyId . partyRow) created))
              ctxt
        (partySortName . partyRow) p' @?= (partySortName . partyRow) p
        -- TODO: explain the values below
        partyPermission p' @?= PermissionADMIN
        partyAccess p' @?= Just (Access { accessSite' = PermissionADMIN, accessMember' = PermissionADMIN }))

unit_partyName_example :: Assertion
unit_partyName_example = do
    (partyName . partyRow) nobodyParty @?= "Everybody"

runLookupParty :: Id Party -> Identity -> Maybe Party -> Assertion
runLookupParty pid ident expected =
    runLookupParty' pid ident id expected

runLookupParty' :: (Show a, Eq a) => Id Party -> Identity -> (Party -> a) -> Maybe a -> Assertion
runLookupParty' pid ident f expected = do
    cn <- loadPGDatabase >>= pgConnect
    let ctxt = TestContext { ctxConn = cn, ctxIdentity = ident }
    --
    mParty <- runReaderT (lookupParty pid :: ReaderT TestContext IO (Maybe Party)) ctxt
    fmap f mParty @?= expected

unit_lookupParty :: Assertion
unit_lookupParty = do
    runLookupParty (Id 2) NotLoggedIn (Just staffParty)
    -- TODO: Fix these with real data parameters
    -- runLookupParty (Id 2) (Identified undefined) (Just staffParty)
    -- runLookupParty (Id 2) (ReIdentified undefined) (Just staffParty)

    runLookupParty' (Id 7) NotLoggedIn (\p -> (partyPermission p, partyAccess p)) (Just (PermissionPUBLIC, Nothing))
    runLookupParty' (Id 7) IdentityNotNeeded (\p -> (partyPermission p, partyAccess p)) (Just (PermissionPUBLIC, Nothing))
    -- runLookupParty' (Id 7) Identified undefined (\p -> (partyPermission p, partyAccess p)) (Just (PermissionPublic, Nothing))
    -- runLookupParty' (Id 7) ReIdentitified undefind (\p -> (partyPermission p, partyAccess p)) (Just (PermissionPublic, Nothing))

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
