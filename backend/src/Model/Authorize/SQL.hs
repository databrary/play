{-# LANGUAGE TemplateHaskell, OverloadedStrings #-}
module Model.Authorize.SQL
  ( authorizationRow
  , authorizeRow
  , selectAuthorizeParent
  , selectAuthorizeChild
  , updateAuthorize
  , insertAuthorize
  , deleteAuthorize
  , selectAuthorizeActivity
  , makeAuthorize
  ) where

import qualified Language.Haskell.TH as TH

import Model.Time
import Model.SQL.Select
import Model.Party.SQL (selectParty)
import Model.Audit.SQL
import Model.Party.Types
import Model.Permission.Types
import Model.Permission.SQL
import Model.Authorize.Types

authorizationRow :: Selector -- ^ @'Party' -> 'Party' -> 'Authorization'@
authorizationRow = selectMap (TH.ConE 'Authorization `TH.AppE`) $ accessRow "authorize_view"

makeAuthorize :: Access -> Maybe Timestamp -> Party -> Party -> Authorize
makeAuthorize a e c p = Authorize
  { authorization = Authorization
    { authorizeAccess = a
    , authorizeChild = c
    , authorizeParent = p
    }
  , authorizeExpires = e
  }

authorizeRow :: Selector -- ^ @'Party' -> 'Party' -> 'Authorize'@
authorizeRow = addSelects 'makeAuthorize
  (accessRow "authorize") [SelectColumn "authorize" "expires"]

selectAuthorizeParent :: TH.Name -- ^ child 'Party'
  -> TH.Name -- ^ 'Identity'
  -> Selector -- ^ 'Authorize'
selectAuthorizeParent child ident = selectJoin '($)
  [ selectMap (`TH.AppE` TH.VarE child) authorizeRow
  , joinOn ("authorize.parent = party.id AND authorize.child = ${partyId $ partyRow " ++ nameRef child ++ "}")
    $ selectParty ident
  ]

selectAuthorizeChild :: TH.Name -- ^ parent 'Party'
  -> TH.Name -- ^ 'Identity'
  -> Selector -- ^ 'Authorize'
selectAuthorizeChild parent ident = selectMap (`TH.AppE` TH.VarE parent) $ selectJoin '($)
  [ authorizeRow
  , joinOn ("authorize.child = party.id AND authorize.parent = ${partyId $ partyRow " ++ nameRef parent ++ "}")
    $ selectParty ident
  ]

authorizeSets :: String -- ^ @'Authorize'@
  -> [(String, String)]
authorizeSets a = accessSets a ++
  [ ("expires", "${authorizeExpires " ++ a ++ "}")
  ]

authorizeKeys :: String -- ^ @'Authorize'@
  -> [(String, String)]
authorizeKeys a =
  [ ("child", "${partyId $ partyRow $ authorizeChild $ authorization " ++ a ++ "}")
  , ("parent", "${partyId $ partyRow $ authorizeParent $ authorization " ++ a ++ "}")
  ]

updateAuthorize :: TH.Name -- ^ @'AuditIdentity'@
  -> TH.Name -- ^ @'Authorize'@
  -> TH.ExpQ
updateAuthorize ident a = auditUpdate ident "authorize"
  (authorizeSets as)
  (whereEq $ authorizeKeys as)
  Nothing
  where as = nameRef a

insertAuthorize :: TH.Name -- ^ @'AuditIdentity'@
  -> TH.Name -- ^ @'Authorize'@
  -> TH.ExpQ
insertAuthorize ident a = auditInsert ident "authorize"
  (authorizeKeys as ++ authorizeSets as)
  Nothing
  where as = nameRef a

deleteAuthorize :: TH.Name -- ^ @'AuditIdentity'@
  -> TH.Name -- ^ @'Authorize'@
  -> TH.ExpQ
deleteAuthorize ident a = auditDelete ident "authorize"
  (whereEq $ authorizeKeys as)
  Nothing
  where as = nameRef a

selectAuthorizeActivity :: TH.Name -- ^@'Identity'@
  -> Selector -- ^ @('Timestamp', 'Party')@
selectAuthorizeActivity ident = selectJoin '(,)
  [ selectAuditActivity "authorize"
  , joinOn "audit.child = party.id"
    $ selectParty ident
  ]
