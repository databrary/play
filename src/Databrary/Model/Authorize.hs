{-# LANGUAGE TemplateHaskell, RecordWildCards, OverloadedStrings, ScopedTypeVariables, DataKinds #-}
module Databrary.Model.Authorize
  ( module Databrary.Model.Authorize.Types
  , selfAuthorize
  , lookupAuthorizedChildren
  , lookupAuthorizedParents
  , AuthorizeFilter(..)
  , lookupAuthorize
  , lookupAuthorizeParent
  , lookupAuthorization
  , changeAuthorize
  , removeAuthorize
  , authorizeExpired
  , authorizeActive
  , authorizeJSON
  , lookupAuthorizeActivity
  , makeAuthorize
  ) where

import Control.Monad (when)
import Data.Int (Int64)
import Data.Maybe (fromMaybe)
import Data.Monoid ((<>))

import Databrary.Has (peek, view)
import qualified Databrary.JSON as JSON
import Databrary.Service.DB
import Databrary.Model.SQL
import Databrary.Model.Time
import Databrary.Model.Id
import Databrary.Model.Audit
import Databrary.Model.Permission
import Databrary.Model.Party
import Databrary.Model.Identity.Types
import Databrary.Model.Authorize.Types
import Databrary.Model.Authorize.SQL

selfAuthorize :: Party -> Authorize
selfAuthorize p =
  Authorize (Authorization (if partyId (partyRow p) == partyId (partyRow nobodyParty) then minBound else maxBound) p p) Nothing

-- | Get authorizations where the given party is the child. When the permission argument has a value,
-- then only provide active, approved authorizations, filtering out authorizations lower than the
-- provided level.
lookupAuthorizedParents :: (MonadDB c m, MonadHasIdentity c m) => Party -> Maybe Permission -> m [Authorize]
lookupAuthorizedParents child perm = do
  -- TODO: specialize the argument to be AuthorizeFilter for this and Children function below
  ident <- peek
  dbQuery $ maybe
    $(selectQuery (selectAuthorizeParent 'child 'ident) "$")
    (\p -> $(selectQuery (selectAuthorizeParent 'child 'ident) "$WHERE (expires IS NULL OR expires > CURRENT_TIMESTAMP) AND site >= ${p} AND member >= ${p} AND (site <> 'NONE' OR member <> 'NONE')"))
    perm

-- | Get authorizations where the given party is the parent. When the permission argument has a value,
-- then only provide active, approved authorizations, filtering out authorizations lower than the
-- provided level.
lookupAuthorizedChildren :: (MonadDB c m, MonadHasIdentity c m) => Party -> Maybe Permission -> m [Authorize]
lookupAuthorizedChildren parent perm = do
  ident <- peek
  dbQuery $ maybe
    $(selectQuery (selectAuthorizeChild 'parent 'ident) "$")
    (\p -> $(selectQuery (selectAuthorizeChild 'parent 'ident) "$WHERE (expires IS NULL OR expires > CURRENT_TIMESTAMP) AND site >= ${p} AND member >= ${p} AND (site <> 'NONE' OR member <> 'NONE')"))
    perm

-- TODO: add combinators above expressing why the filters are being used, probably in authorize controller
data AuthorizeFilter = AllAuthorizations | ActiveAuthorizations deriving (Eq, Show)

-- | Attempt to find an authorization request or grant from the child party to the granting parent party.
-- If authorize filter is ForPartyViewer, filter out expired authorizations.
lookupAuthorize :: MonadDB c m => AuthorizeFilter -> Party -> Party -> m (Maybe Authorize)
lookupAuthorize aFilter child parent =
  dbQuery1 $
      (\mkAuthorize' -> mkAuthorize' child parent)
          <$> case aFilter of
                  ActiveAuthorizations ->
                      $(selectQuery
                            authorizeRow
                            "$WHERE authorize.child = ${partyId $ partyRow child} AND authorize.parent = ${partyId $ partyRow parent} AND (expires IS NULL OR expires > CURRENT_TIMESTAMP)")
                  AllAuthorizations ->
                      $(selectQuery
                            authorizeRow
                            "$WHERE authorize.child = ${partyId $ partyRow child} AND authorize.parent = ${partyId $ partyRow parent}")

-- | Find an active authorization request or approval from child to parent.
lookupAuthorizeParent :: (MonadDB c m, MonadHasIdentity c m) => Party -> Id Party -> m (Maybe Authorize)
lookupAuthorizeParent child parent = do
  ident <- peek
  dbQuery1 $ $(selectQuery (selectAuthorizeParent 'child 'ident) "$WHERE authorize.parent = ${parent} AND (expires IS NULL OR expires > CURRENT_TIMESTAMP)")

-- | Get the core active authorization entry between a child and parent, after inheritance has been applied.
-- Override authorize_view for the corner case of nobody as both parent and child.
lookupAuthorization :: (MonadDB c m, MonadHasIdentity c m) => Party -> Party -> m Authorization
lookupAuthorization child parent
  | partyId (partyRow child) == partyId (partyRow parent) = return $ authorization $ selfAuthorize child
  | otherwise = do
    auth <- peek
    if partyId (view auth) == partyId (partyRow child) && partyId (partyRow parent) == partyId (partyRow rootParty)
      then return $ Authorization (siteAccess auth) child parent -- short circuit to get already fetched value in siteauthx
      else fromMaybe (Authorization mempty child parent) <$> -- if not valid entry found, assume no access
        dbQuery1 ((\a -> a child parent) <$> $(selectQuery authorizationRow "!$WHERE authorize_view.child = ${partyId $ partyRow child} AND authorize_view.parent = ${partyId $ partyRow parent}"))

-- | Update or insert the authorization object. Use the request and identity context to log the change in the
-- corresponding audit table as well.
changeAuthorize :: (MonadAudit c m) => Authorize -> m ()
changeAuthorize auth = do
  ident <- getAuditIdentity
  (r, _) <- updateOrInsert
    $(updateAuthorize 'ident 'auth)
    $(insertAuthorize 'ident 'auth)
  when (r /= 1) $ fail $ "changeAuthorize: " ++ show r ++ " rows"

removeAuthorize :: (MonadAudit c m) => Authorize -> m Bool
removeAuthorize auth = do
  ident <- getAuditIdentity
  dbExecute1 $(deleteAuthorize 'ident 'auth)

authorizationActive :: Authorization -> Bool
authorizationActive Authorization{ authorizeAccess = a } = a /= mempty

authorizeExpired :: Authorize -> Timestamp -> Bool
authorizeExpired Authorize{ authorizeExpires = Just e } = (e <)
authorizeExpired _ = const False

authorizeActive :: Authorize -> Timestamp -> Bool
authorizeActive a t = authorizationActive (authorization a) && not (authorizeExpired a t)

authorizeJSON :: JSON.ToObject o => Authorize -> o
authorizeJSON Authorize{..} = accessJSON (authorizeAccess authorization)
  <> "expires" `JSON.kvObjectOrEmpty` authorizeExpires

lookupAuthorizeActivity :: (MonadDB c m, MonadHasIdentity c m) => Int -> m [(Timestamp, Party)]
lookupAuthorizeActivity limit = do
  ident :: Identity <- peek
  dbQuery $(selectQuery (selectAuthorizeActivity 'ident) "$JOIN authorize_view ON audit.parent = authorize_view.child AND authorize_view.parent = 0 WHERE audit.audit_action IN ('add','change') AND audit.site >= 'EDIT' AND authorize_view.site > 'EDIT' ORDER BY audit.audit_time DESC LIMIT ${fromIntegral limit :: Int64}")
