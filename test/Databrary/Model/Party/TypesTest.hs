{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module Databrary.Model.Party.TypesTest where

import qualified Data.ByteString as BS
import Data.Maybe (fromJust)
import qualified Data.Text as T
import Hedgehog
import Hedgehog.Gen as Gen
import Hedgehog.Range as Range
import Network.URI
import Test.Tasty
-- import Test.Tasty.Hedgehog

import Databrary.Model.Permission.Types
import Databrary.Model.Party.Types
import Databrary.Model.Id.Types

genPartyId :: Gen (Id Party)
genPartyId = Id <$> Gen.integral (Range.constant 3 5000)

genPartySortName :: Gen T.Text
genPartySortName = Gen.text (Range.constant 0 80) Gen.alpha

genPartyPreName :: Gen T.Text
genPartyPreName = Gen.text (Range.constant 0 80) Gen.alpha

genPartyAffiliation :: Gen T.Text
genPartyAffiliation = Gen.text (Range.constant 0 150) Gen.alpha

genPartyRowSimple :: Gen PartyRow
genPartyRowSimple =
    PartyRow
        <$> genPartyId
        <*> genPartySortName
        <*> Gen.maybe genPartyPreName
        <*> pure Nothing
        <*> Gen.maybe genPartyAffiliation
        <*> pure Nothing
-- TODO: split into group, ai, collaborator, lab manager, lab staff

genInstitutionUrl :: Gen (Maybe URI)
genInstitutionUrl =
    Just <$> pure ((fromJust . parseURI) "https://www.nyu.edu")

genInstitutionPartyRow :: Gen PartyRow
genInstitutionPartyRow = do
    PartyRow
        <$> genPartyId
        <*> genPartySortName
        <*> Gen.maybe (pure "The")
        <*> pure Nothing -- only for researchers, not institutions
        <*> pure Nothing
        <*> genInstitutionUrl

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

genAccountEmail :: Gen BS.ByteString
genAccountEmail = pure "adam.smith@nyu.edu"

genPartySimple :: Gen Party
genPartySimple = do
   let gPerm = pure PermissionPUBLIC
   let gAccess = pure Nothing
   p <- Party <$> genPartyRowSimple <*> pure Nothing <*> gPerm <*> gAccess
   a <- Account <$> genAccountEmail <*> pure p
   (let p2 = p { partyAccount = Just a2 } -- account expected below
        a2 = a { accountParty = p2 }
    in pure p2)

genInstitutionParty :: Gen Party
genInstitutionParty = do
   let gPerm = pure PermissionPUBLIC
       gAccess = pure Nothing
   -- what are the typical values for access and permission for an institution?
   Party <$> genInstitutionPartyRow <*> pure Nothing <*> gPerm <*> gAccess

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
