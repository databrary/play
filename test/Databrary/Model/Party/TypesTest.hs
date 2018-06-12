{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module Databrary.Model.Party.TypesTest where

-- import qualified Data.ByteString as BS
-- import Data.Maybe (fromJust)
-- import Data.Monoid ((<>))
-- import qualified Data.Text as T
-- import qualified Data.Text.Encoding as TE
-- import Network.URI

import Databrary.Model.Permission.Types
import Databrary.Model.Party.Types
import Databrary.Model.Id.Types

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

