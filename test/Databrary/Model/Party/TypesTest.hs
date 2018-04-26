{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module Databrary.Model.Party.TypesTest where

import Data.Maybe (fromJust)
import Hedgehog
import Hedgehog.Gen as Gen
import Hedgehog.Range as Range
import Test.Tasty
-- import Test.Tasty.Hedgehog

import Databrary.Model.Permission.Types
import Databrary.Model.Party.Types
import Databrary.Model.Id.Types

genPartyRowSimple :: Gen PartyRow
genPartyRowSimple =
    PartyRow
        <$> (Id <$> Gen.integral (Range.constant 3 5000))
        <*> Gen.text (Range.constant 0 80) Gen.alpha
        <*> (Just <$> Gen.text (Range.constant 0 80) Gen.alpha)
        <*> pure Nothing
        <*> (Just <$> Gen.text (Range.constant 0 150) Gen.alpha)
        <*> pure Nothing
-- TODO: split into institution, group, ai, collaborator, lab manager, lab staff

partyRow1 :: PartyRow
partyRow1 =
    PartyRow {
          partyId = Id 10
        , partySortName = "Smith"
        , partyPreName = Just "John"
        , partyORCID = Nothing
        , partyAffiliation = Just "New York University"
        , partyURL = Nothing
        }

genPartySimple :: Gen Party
genPartySimple = do
   let gRow = genPartyRowSimple
   let gPerm = pure PermissionPUBLIC
   let gAcc = pure Nothing
   p <- Party <$> gRow <*> pure Nothing <*> gPerm <*> gAcc
   a <- Account <$> pure "adam.smith@nyu.edu" <*> pure p
   (let p2 = p { partyAccount = Just a2 } -- account expected below
        a2 = a { accountParty = p2 }
    in pure p2)

party1 :: Party
party1 =
    let
        p = 
            Party {
                  partyRow = partyRow1
                , partyAccount = Just a
                , partyPermission = PermissionPUBLIC
                , partyAccess = -- what are typical values here?
                      Just
                          (Access { accessSite' = PermissionSHARED, accessMember' = PermissionSHARED })
                }
        a =
            Account {
                  accountEmail = "john.smith@nyu.edu"
                , accountParty = p
                }
    in
        p

genSiteAuthSimple :: Gen SiteAuth
genSiteAuthSimple = do
    p <- genPartySimple
    ac <- Access <$> pure PermissionSHARED <*> pure PermissionSHARED
    SiteAuth
        <$> (pure . fromJust . partyAccount) p
        <*> Just <$> (Gen.utf8 (Range.constant 6 20) Gen.ascii)
        <*> pure ac

test_all :: [TestTree]
test_all =
    [
    ]
