{-# LANGUAGE TemplateHaskell, DataKinds #-}
module Databrary.Model.Party.Boot
  ( loadParty
  ) where

import qualified Language.Haskell.TH as TH
import qualified Language.Haskell.TH.Syntax as TH

import Databrary.Service.DB
import Databrary.Model.SQL (selectQuery)
import Databrary.Model.SQL.Select
import Databrary.Model.Id.Types
import Databrary.Model.Permission.Types
import Databrary.Model.Party.Types
import Databrary.Model.Party.SQL

loadParty :: Id Party -> Permission -> TH.ExpQ -- ^ @'Party'@
loadParty i perm = do
  p <- runTDB $ dbQuery1' $(selectQuery (selectColumns 'PartyRow "party" ["id", "name", "prename", "orcid", "affiliation", "url"]) "WHERE party.id = ${i}")
  TH.lift $ Party p Nothing perm Nothing
