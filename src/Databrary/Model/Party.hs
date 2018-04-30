{-# LANGUAGE OverloadedStrings, TemplateHaskell, QuasiQuotes, RecordWildCards, DataKinds #-}
module Databrary.Model.Party
  ( module Databrary.Model.Party.Types
  , nobodyParty
  , rootParty
  , staffParty
  , partyName
  , partyEmail
  , lookupParty
  , lookupPartyAuthorizations
  , lookupAuthParty
  , lookupSiteAuthByEmail
  , changeParty
  , changeAccount
  , addParty
  , addAccount
  , removeParty
  , auditAccountLogin
  , recentAccountLogins
  , partyRowJSON
  , partyJSON
  , PartyFilter(..)
  , findParties
  , lookupAvatar
  , changeAvatar
  , getDuplicateParties
  ) where

import Control.Applicative ((<|>))
import Control.Exception.Lifted (handleJust)
import Control.Monad (guard)
import qualified Data.ByteString as BS
import Data.Int (Int64)
import Data.List (intercalate)
import Data.Maybe (isNothing, fromMaybe)
import Data.Monoid ((<>))
import qualified Data.String
import qualified Data.Text as T
import Database.PostgreSQL.Typed (pgSQL)
import Database.PostgreSQL.Typed.Query (unsafeModifyQuery)
import Database.PostgreSQL.Typed.Dynamic (pgLiteralRep, pgLiteralString, pgSafeLiteral)
import Database.PostgreSQL.Typed.Types

import Databrary.Ops
import Databrary.Has (Has(..), peek)
import Databrary.Service.DB
import qualified Databrary.JSON as JSON
import Databrary.HTTP.Request
import Databrary.Model.Id
import Databrary.Model.SQL
import Databrary.Model.Paginate
import Databrary.Model.Paginate.SQL
import Databrary.Model.Permission
import Databrary.Model.Audit
import Databrary.Model.Audit.SQL
import Databrary.Model.Identity.Types
import Databrary.Model.Volume
import Databrary.Model.Asset.Types
import Databrary.Model.Asset.SQL
import Databrary.Model.Party.Types
import Databrary.Model.Party.SQL

useTDB

nobodyParty, rootParty, staffParty :: Party -- TODO: load on startup from service module
nobodyParty =
   Party
         (PartyRow (Id (-1)) (T.pack "Everybody") Nothing Nothing Nothing Nothing)
         Nothing
         PermissionREAD
         Nothing
rootParty =
      Party
         (PartyRow (Id 0) (T.pack "Databrary") Nothing Nothing Nothing Nothing)
         Nothing
         PermissionSHARED
         Nothing
staffParty =
   Party
         (PartyRow (Id 2) (T.pack "Staff") Nothing Nothing (Just (T.pack "Databrary")) Nothing)
         Nothing
         PermissionPUBLIC
         Nothing

partyName :: PartyRow -> T.Text
partyName PartyRow{ partyPreName = Just p, partySortName = n } = p <> T.cons ' ' n
partyName PartyRow{ partySortName = n } = n

emailPermission :: Permission
emailPermission = PermissionSHARED

showEmail :: Identity -> Bool
showEmail i = accessSite i >= emailPermission

partyEmail :: Party -> Maybe BS.ByteString
partyEmail p =
  guard (partyPermission p >= emailPermission) >> accountEmail <$> partyAccount p

partyRowJSON :: JSON.ToObject o => PartyRow -> JSON.Record (Id Party) o
partyRowJSON PartyRow{..} = JSON.Record partyId $
     "sortname" JSON..= partySortName
  <> "prename" `JSON.kvObjectOrEmpty` partyPreName
  <> "orcid" `JSON.kvObjectOrEmpty` (show <$> partyORCID)
  <> "affiliation" `JSON.kvObjectOrEmpty` partyAffiliation
  <> "url" `JSON.kvObjectOrEmpty` partyURL

partyJSON :: JSON.ToObject o => Party -> JSON.Record (Id Party) o
partyJSON p@Party{..} = partyRowJSON partyRow `JSON.foldObjectIntoRec`
 (   "institution" `JSON.kvObjectOrEmpty` (True <? isNothing partyAccount)
  <> "email" `JSON.kvObjectOrEmpty` partyEmail p
  <> "permission" `JSON.kvObjectOrEmpty` (partyPermission <? partyPermission > PermissionREAD))

changeParty :: MonadAudit c m => Party -> m ()
changeParty p = do
  ident <- getAuditIdentity
  let _tenv_a6PEM = unknownPGTypeEnv
  dbExecute1' -- (updateParty 'ident 'p)
   (mapQuery2
    ((\ _p_a6PEN _p_a6PEO _p_a6PEP _p_a6PEQ _p_a6PER _p_a6PES _p_a6PET ->
                    (BS.concat
                       [Data.String.fromString
                          "WITH audit_row AS (UPDATE party SET name=",
                        Database.PostgreSQL.Typed.Types.pgEscapeParameter
                          _tenv_a6PEM
                          (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                             Database.PostgreSQL.Typed.Types.PGTypeName "text")
                          _p_a6PEN,
                        Data.String.fromString ",prename=",
                        Database.PostgreSQL.Typed.Types.pgEscapeParameter
                          _tenv_a6PEM
                          (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                             Database.PostgreSQL.Typed.Types.PGTypeName "text")
                          _p_a6PEO,
                        Data.String.fromString ",affiliation=",
                        Database.PostgreSQL.Typed.Types.pgEscapeParameter
                          _tenv_a6PEM
                          (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                             Database.PostgreSQL.Typed.Types.PGTypeName "text")
                          _p_a6PEP,
                        Data.String.fromString ",url=",
                        Database.PostgreSQL.Typed.Types.pgEscapeParameter
                          _tenv_a6PEM
                          (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                             Database.PostgreSQL.Typed.Types.PGTypeName "text")
                          _p_a6PEQ,
                        Data.String.fromString " WHERE id=",
                        Database.PostgreSQL.Typed.Types.pgEscapeParameter
                          _tenv_a6PEM
                          (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                             Database.PostgreSQL.Typed.Types.PGTypeName "integer")
                          _p_a6PER,
                        Data.String.fromString
                          " RETURNING *) INSERT INTO audit.party SELECT CURRENT_TIMESTAMP, ",
                        Database.PostgreSQL.Typed.Types.pgEscapeParameter
                          _tenv_a6PEM
                          (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                             Database.PostgreSQL.Typed.Types.PGTypeName "integer")
                          _p_a6PES,
                        Data.String.fromString ", ",
                        Database.PostgreSQL.Typed.Types.pgEscapeParameter
                          _tenv_a6PEM
                          (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                             Database.PostgreSQL.Typed.Types.PGTypeName "inet")
                          _p_a6PET,
                        Data.String.fromString
                          ", 'change'::audit.action, * FROM audit_row"]))
      (partySortName $ partyRow p)
      (partyPreName $ partyRow p)
      (partyAffiliation $ partyRow p)
      (partyURL $ partyRow p)
      (partyId $ partyRow p)
      (auditWho ident)
      (auditIp ident))
    (\ [] -> ()))

changeAccount :: MonadAudit c m => SiteAuth -> m ()
changeAccount a = do
  ident <- getAuditIdentity
  let _tenv_a6PFv = unknownPGTypeEnv
  dbExecute1' -- (updateAccount 'ident 'a)
   (mapQuery2
    ((\ _p_a6PFw _p_a6PFx _p_a6PFy _p_a6PFz _p_a6PFA ->
                    (BS.concat
                       [Data.String.fromString
                          "WITH audit_row AS (UPDATE account SET email=",
                        Database.PostgreSQL.Typed.Types.pgEscapeParameter
                          _tenv_a6PFv
                          (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                             Database.PostgreSQL.Typed.Types.PGTypeName "character varying")
                          _p_a6PFw,
                        Data.String.fromString ",password=",
                        Database.PostgreSQL.Typed.Types.pgEscapeParameter
                          _tenv_a6PFv
                          (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                             Database.PostgreSQL.Typed.Types.PGTypeName "character varying")
                          _p_a6PFx,
                        Data.String.fromString " WHERE id=",
                        Database.PostgreSQL.Typed.Types.pgEscapeParameter
                          _tenv_a6PFv
                          (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                             Database.PostgreSQL.Typed.Types.PGTypeName "integer")
                          _p_a6PFy,
                        Data.String.fromString
                          " RETURNING *) INSERT INTO audit.account SELECT CURRENT_TIMESTAMP, ",
                        Database.PostgreSQL.Typed.Types.pgEscapeParameter
                          _tenv_a6PFv
                          (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                             Database.PostgreSQL.Typed.Types.PGTypeName "integer")
                          _p_a6PFz,
                        Data.String.fromString ", ",
                        Database.PostgreSQL.Typed.Types.pgEscapeParameter
                          _tenv_a6PFv
                          (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                             Database.PostgreSQL.Typed.Types.PGTypeName "inet")
                          _p_a6PFA,
                        Data.String.fromString
                          ", 'change'::audit.action, * FROM audit_row"]))
      (accountEmail (siteAccount a))
      (accountPasswd a)
      (partyId $ partyRow (accountParty (siteAccount a)))
      (auditWho ident)
      (auditIp ident))
    (\[] -> ()))

addParty :: MonadAudit c m => Party -> m Party
addParty bp = do
  ident <- getAuditIdentity
  dbQuery1' $ fmap (\p -> Party p Nothing PermissionREAD Nothing) $(insertParty 'ident 'bp)

addAccount :: MonadAudit c m => Account -> m Account
addAccount ba@Account{ accountParty = bp } = do
  ident <- getAuditIdentity
  p <- dbQuery1' $ fmap (\p -> Party p Nothing PermissionREAD Nothing) $(insertParty 'ident 'bp)
  let pa = p{ partyAccount = Just a }
      a = ba{ accountParty = pa }
  dbExecute1' $(insertAccount 'ident 'a)
  return a

removeParty :: MonadAudit c m => Party -> m Bool
removeParty p = do
  ident <- getAuditIdentity
  dbTransaction $ handleJust (guard . isForeignKeyViolation) (\_ -> return False) $ do
    _ <- dbExecute1 $(deleteAccount 'ident 'p)
    dbExecute1 $(deleteParty 'ident 'p)

lookupFixedParty :: Id Party -> Identity -> Maybe Party
lookupFixedParty (Id (-1)) _ = Just nobodyParty
lookupFixedParty (Id 0) i =
  Just rootParty{
    partyPermission = accessPermission i `max` PermissionSHARED
  , partyAccess = (accessMember i > PermissionNONE) `thenUse` (view i) }
lookupFixedParty i a = view a <? (i == view a)

-- | Given the id for a party, ensure ... and resolve the id to the full party object. The produced party has permissions
-- for the retrieving viewer baked in.
lookupParty :: (MonadDB c m, MonadHasIdentity c m) => Id Party -> m (Maybe Party)
lookupParty i = do
  ident <- peek
  lookupFixedParty i ident `orElseM`
    dbQuery1 $(selectQuery (selectParty 'ident) "$WHERE party.id = ${i}")

getDuplicateParties :: MonadDB c m => m [PartyRow]
getDuplicateParties = do
  dbQuery
    $(selectQuery (selectPartyRow)
        "$WHERE exists \
        \ (select * \
        \  from party p2 \
        \  where p2.prename = party.prename and p2.name = party.name and party.id < p2.id) ")

lookupPartyAuthorizations :: (MonadDB c m, MonadHasIdentity c m) => m [(Party, Maybe Permission)]
lookupPartyAuthorizations = do
  ident <- peek
  let _tenv_a6Qkm = unknownPGTypeEnv
  rows <- dbQuery -- (selectQuery (selectPartyAuthorization 'ident) "WHERE party.id > 0")
   (mapQuery2
                      (BS.concat
                         [Data.String.fromString
                            "SELECT party.id,party.name,party.prename,party.orcid,party.affiliation,party.url,account.email,authorize_view.site,authorize_view.member FROM party LEFT JOIN account USING (id) LEFT JOIN authorize_view ON party.id = authorize_view.child AND authorize_view.parent = 0 WHERE party.id > 0"])
              (\
                 [_cid_a6Qkn,
                  _cname_a6Qko,
                  _cprename_a6Qkp,
                  _corcid_a6Qkq,
                  _caffiliation_a6Qkr,
                  _curl_a6Qks,
                  _cemail_a6Qkt,
                  _csite_a6Qku,
                  _cmember_a6Qkv]
                 -> (Database.PostgreSQL.Typed.Types.pgDecodeColumnNotNull
                       _tenv_a6Qkm
                       (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                          Database.PostgreSQL.Typed.Types.PGTypeName "integer")
                       _cid_a6Qkn, 
                     Database.PostgreSQL.Typed.Types.pgDecodeColumnNotNull
                       _tenv_a6Qkm
                       (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                          Database.PostgreSQL.Typed.Types.PGTypeName "text")
                       _cname_a6Qko, 
                     Database.PostgreSQL.Typed.Types.pgDecodeColumn
                       _tenv_a6Qkm
                       (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                          Database.PostgreSQL.Typed.Types.PGTypeName "text")
                       _cprename_a6Qkp, 
                     Database.PostgreSQL.Typed.Types.pgDecodeColumn
                       _tenv_a6Qkm
                       (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                          Database.PostgreSQL.Typed.Types.PGTypeName "bpchar")
                       _corcid_a6Qkq, 
                     Database.PostgreSQL.Typed.Types.pgDecodeColumn
                       _tenv_a6Qkm
                       (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                          Database.PostgreSQL.Typed.Types.PGTypeName "text")
                       _caffiliation_a6Qkr, 
                     Database.PostgreSQL.Typed.Types.pgDecodeColumn
                       _tenv_a6Qkm
                       (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                          Database.PostgreSQL.Typed.Types.PGTypeName "text")
                       _curl_a6Qks, 
                     Database.PostgreSQL.Typed.Types.pgDecodeColumnNotNull
                       _tenv_a6Qkm
                       (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                          Database.PostgreSQL.Typed.Types.PGTypeName "character varying")
                       _cemail_a6Qkt, 
                     Database.PostgreSQL.Typed.Types.pgDecodeColumn
                       _tenv_a6Qkm
                       (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                          Database.PostgreSQL.Typed.Types.PGTypeName "permission")
                       _csite_a6Qku, 
                     Database.PostgreSQL.Typed.Types.pgDecodeColumn
                       _tenv_a6Qkm
                       (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                          Database.PostgreSQL.Typed.Types.PGTypeName "permission")
                       _cmember_a6Qkv)))
  pure 
     (fmap
      (\ (vid_a6Qir, vname_a6Qis, vprename_a6Qit, vorcid_a6Qiu,
          vaffiliation_a6Qiv, vurl_a6Qiw, vemail_a6Qix, vsite_a6Qiz,
          vmember_a6QiA)
         -> Databrary.Model.Party.SQL.makePartyAuthorization
              (Databrary.Model.Party.SQL.permissionParty
                 (Databrary.Model.Party.SQL.makeParty
                    (PartyRow
                       vid_a6Qir
                       vname_a6Qis
                       vprename_a6Qit
                       vorcid_a6Qiu
                       vaffiliation_a6Qiv
                       vurl_a6Qiw)
                    (do { cm_a6QiP <- vemail_a6Qix;
                          Just (Account cm_a6QiP) }))
                 Nothing
                 ident)
              (do { cm_a6QiV <- vsite_a6Qiz;
                    cm_a6QiW <- vmember_a6QiA;
                    Just (Access cm_a6QiV cm_a6QiW) }))
      rows)

lookupAuthParty :: (MonadDB c m, MonadHasIdentity c m) => Id Party -> m (Maybe Party)
lookupAuthParty i = do
  ident <- peek
  lookupFixedParty i ident `orElseM`
    dbQuery1 $(selectQuery (selectAuthParty 'ident) "$WHERE party.id = ${i}")

-- | resolve email to its party and enclosing account and site authenticated identity, possibly case insensitive
lookupSiteAuthByEmail
    :: MonadDB c m
    => Bool -- ^ be case-insensitive?
    -> BS.ByteString
    -> m (Maybe SiteAuth)
lookupSiteAuthByEmail caseInsensitive e = do
  r <- dbQuery1 $(selectQuery selectSiteAuth "WHERE account.email = ${e}")
  if caseInsensitive && isNothing r
    then do
      a <- dbQuery $(selectQuery selectSiteAuth "WHERE lower(account.email) = lower(${e}) LIMIT 2")
      return $ case a of
        [x] -> Just x
        _ -> Nothing
    else
      return r

auditAccountLogin :: (MonadHasRequest c m, MonadDB c m) => Bool -> Party -> BS.ByteString -> m ()
auditAccountLogin success who email = do
  ip <- getRemoteIp
  dbExecute1' [pgSQL|INSERT INTO audit.account (audit_action, audit_user, audit_ip, id, email) VALUES
    (${if success then AuditActionOpen else AuditActionAttempt}, -1, ${ip}, ${partyId $ partyRow who}, ${email})|]

recentAccountLogins :: MonadDB c m => Party -> m Int64
recentAccountLogins who = fromMaybe 0 <$>
  dbQuery1 [pgSQL|!SELECT count(*) FROM audit.account WHERE audit_action = 'attempt' AND id = ${partyId $ partyRow who} AND audit_time > CURRENT_TIMESTAMP - interval '1 hour'|]

data PartyFilter = PartyFilter
  { partyFilterQuery :: Maybe String
  , partyFilterAuthorization :: Maybe Permission
  , partyFilterInstitution :: Maybe Bool
  , partyFilterPaginate :: Paginate
  }

instance Monoid PartyFilter where
  mempty = PartyFilter Nothing Nothing Nothing def
  mappend (PartyFilter q1 a1 i1 p) (PartyFilter q2 a2 i2 _) =
    PartyFilter (q1 <> q2) (a1 <|> a2) (i1 <|> i2) p

partyFilter :: PartyFilter -> Identity -> BS.ByteString
partyFilter PartyFilter{..} ident = BS.concat
  [ withq partyFilterAuthorization (const " JOIN authorize_view ON party.id = child AND parent = 0")
  , " WHERE id > 0 AND id != ", pgLiteralRep (partyId $ partyRow $ staffParty)
  , withq partyFilterQuery (\n -> " AND " <> queryVal <> " ILIKE " <> pgLiteralRep (wordPat n))
  , withq partyFilterAuthorization (\a -> " AND site = " <> pgSafeLiteral a)
  , withq partyFilterInstitution (\i -> if i then " AND account.id IS NULL" else " AND account.password IS NOT NULL")
  , " ORDER BY name, prename "
  , paginateSQL partyFilterPaginate
  ]
  where
  withq v f = maybe "" f v
  wordPat = intercalate "%" . ("":) . (++[""]) . words
  queryVal
    | showEmail ident = "(COALESCE(prename || ' ', '') || name || COALESCE(' ' || email, ''))"
    | otherwise = "(COALESCE(prename || ' ', '') || name)"

findParties :: (MonadHasIdentity c m, MonadDB c m) => PartyFilter -> m [Party]
findParties pf = do
  ident <- peek
  dbQuery $ unsafeModifyQuery $(selectQuery (selectParty 'ident) "")
    (<> partyFilter pf ident)

lookupAvatar :: MonadDB c m => Id Party -> m (Maybe Asset)
lookupAvatar p =
  dbQuery1 $ (`Asset` coreVolume) <$> $(selectQuery selectAssetRow $ "$JOIN avatar ON asset.id = avatar.asset WHERE avatar.party = ${p} AND asset.volume = " ++ pgLiteralString (volumeId $ volumeRow coreVolume))

changeAvatar :: MonadAudit c m => Party -> Maybe Asset -> m Bool
changeAvatar p Nothing = do
  ident <- getAuditIdentity
  dbExecute1 $(auditDelete 'ident "avatar" "party = ${partyId $ partyRow p}" Nothing)
changeAvatar p (Just a) = do
  ident <- getAuditIdentity
  (0 <) . fst <$> updateOrInsert
    $(auditUpdate 'ident "avatar" [("asset", "${assetId $ assetRow a}")] "party = ${partyId $ partyRow p}" Nothing)
    $(auditInsert 'ident "avatar" [("asset", "${assetId $ assetRow a}"), ("party", "${partyId $ partyRow p}")] Nothing)
