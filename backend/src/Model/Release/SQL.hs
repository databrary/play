{-# LANGUAGE OverloadedStrings #-}
module Model.Release.SQL
  ( releaseRow
  , insertRelease
  , updateRelease
  , deleteRelease
  ) where

import qualified Language.Haskell.TH as TH

import Model.SQL.Select
import Model.Audit.SQL
import Model.Slot.SQL

releaseRow :: Selector -- ^ @'Release'@
releaseRow = selectColumn "slot_release" "release"

releaseSets :: String -- ^ @'Release'@
  -> [(String, String)]
releaseSets o =
  [ ("release", "${" ++ o ++ "}")
  ]

insertRelease :: TH.Name -- ^ @'AuditIdentity'@
  -> TH.Name -- ^ @'Slot'@
  -> TH.Name -- ^ @'Release'@
  -> TH.ExpQ
insertRelease ident s c = auditInsert ident "slot_release"
  (slotKeys (nameRef s) ++ releaseSets (nameRef c))
  Nothing

updateRelease :: TH.Name -- ^ @'AuditIdentity'@
  -> TH.Name -- ^ @'Slot'@
  -> TH.Name -- ^ @'Release'@
  -> TH.ExpQ -- ^ @'Release'@
updateRelease ident s c = auditUpdate ident "slot_release"
  (releaseSets (nameRef c))
  (whereEq $ slotKeys (nameRef s))
  Nothing

deleteRelease :: TH.Name -- ^ @'AuditIdentity'@
  -> TH.Name -- ^ @'Slot'@
  -> TH.ExpQ -- ^ @()@
deleteRelease ident o = auditDelete ident "slot_release"
  ("container = ${containerId $ containerRow $ slotContainer " ++ os ++ "} AND segment <@ ${slotSegment " ++ os ++ "}")
  Nothing
  where os = nameRef o
