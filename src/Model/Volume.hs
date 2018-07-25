{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
module Model.Volume
    ( module Model.Volume.Types
    , coreVolume
    , lookupVolume
    , lookupVolumeP
    , changeVolume
    , addVolume
    , auditVolumeDownload
    , VolumeFilter(..)
    , findVolumes
    , getVolumeAlias
    , volumeRowJSON
    , volumeJSON
    , volumeJSONSimple
    , updateVolumeIndex
    ) where

import Control.Applicative ((<|>))
import Control.Monad (guard)
import Data.List (find)
import Data.Monoid ((<>))
import Data.Semigroup hiding ((<>))
import Database.PostgreSQL.Typed.Dynamic (pgLiteralRep)
import Database.PostgreSQL.Typed.Query (pgSQL, unsafeModifyQuery)
import qualified Data.ByteString as BS
import qualified Data.Text as T
import qualified Data.Time as TM
import qualified Data.Semigroup

import Has (peek, view)
import Model.Audit
import Model.Id
import Model.Identity.Types
import Model.Paginate (Paginate (..), def)
import Model.Paginate.SQL (paginateSQL)
import Model.Party.Types
import Model.Permission
import Model.SQL (selectQuery)
import Model.Volume.SQL
import Model.Volume.Types
import Model.VolumeAccess.Types (VolumeAccess(..))
import Service.DB
import qualified JSON

coreVolume :: Volume
-- TODO: load on startup in lookups service module
coreVolume = Volume
    (VolumeRow (Id 0) "Core" Nothing Nothing Nothing)
    (TM.UTCTime
        (TM.ModifiedJulianDay 56303)
        (TM.picosecondsToDiffTime 37600000000000000)
    )
    []
    (RolePublicViewer PublicNoPolicy)

-- | Lookup a 'Volume' by its Id, and wrap it in 'Permissioned'. The plan is for
-- this to replace 'lookupVolume' entirely.
lookupVolumeP
    :: (MonadDB c m, MonadHasIdentity c m)
    => Id Volume
    -> m (Maybe (Permissioned Volume))
lookupVolumeP =
    fmap
            (fmap
                (Permissioned
                <*> extractPermissionIgnorePolicy . volumeRolePolicy
                )
            )
        . lookupVolume

lookupVolume
    :: (MonadDB c m, MonadHasIdentity c m) => Id Volume -> m (Maybe Volume)
lookupVolume vi = do
    ident :: Identity <- peek
    dbQuery1 $(selectQuery (selectVolume 'ident) "$WHERE volume.id = ${vi}")

changeVolume :: MonadAudit c m => Volume -> m ()
changeVolume v = do
    ident <- getAuditIdentity
    dbExecute1' $(updateVolume 'ident 'v)

addVolume :: MonadAudit c m => Volume -> m Volume
addVolume bv = do
    ident <- getAuditIdentity
    dbQuery1' $ fmap (\v -> v [] RoleAdmin) $(insertVolume 'ident 'bv)

getVolumeAlias :: Volume -> Maybe T.Text
getVolumeAlias v = do
    guard
        (extractPermissionIgnorePolicy (volumeRolePolicy v) >= PermissionREAD)
    volumeAlias (volumeRow v)

auditVolumeDownload :: MonadAudit c m => Bool -> Volume -> m ()
auditVolumeDownload success vol = do
    ai <- getAuditIdentity
    dbExecute1'
        [pgSQL|$
            INSERT INTO audit.volume (audit_action, audit_user, audit_ip, id)
            VALUES (
                ${if success then AuditActionOpen else AuditActionAttempt},
                ${auditWho ai},
                ${auditIp ai},
                ${volumeId $ volumeRow vol})
        |]

volumeRowJSON :: JSON.ToObject o => VolumeRow -> JSON.Record (Id Volume) o
volumeRowJSON VolumeRow {..} =
    JSON.Record volumeId
        $ "name"
        JSON..= volumeName
        <> "body"
        JSON..= volumeBody

volumeJSON
    :: JSON.ToObject o
    => Volume
    -> Maybe [VolumeAccess]
    -> JSON.Record (Id Volume) o
volumeJSON v@Volume{..} mAccesses =
    JSON.foldObjectIntoRec
        (volumeRowJSON volumeRow)
        ("doi" `JSON.kvObjectOrEmpty` volumeDOI volumeRow
        <> "alias" `JSON.kvObjectOrEmpty` getVolumeAlias v
        <> "creation" JSON..= volumeCreation
        <> "owners" JSON..=
            map (\(i, n) -> JSON.Object ("id" JSON..= i <> "name" JSON..= n))
                volumeOwners
        <> "permission" JSON..= extractPermissionIgnorePolicy volumeRolePolicy
        <> "publicsharefull" JSON..= volumeAccessPolicyJSON v
        <> "publicaccess" `JSON.kvObjectOrEmpty`
            fmap (show . volumePublicAccessSummary) mAccesses)

volumeJSONSimple :: JSON.ToObject o => Volume -> JSON.Record (Id Volume) o
volumeJSONSimple v = volumeJSON v Nothing

volumeAccessPolicyJSON :: Volume -> Maybe Bool
volumeAccessPolicyJSON v = case volumeRolePolicy v of
    RolePublicViewer PublicRestrictedPolicy -> Just False
    RoleSharedViewer SharedRestrictedPolicy -> Just False
    RolePublicViewer PublicNoPolicy -> Just True
    _ -> Nothing

data VolumeFilter = VolumeFilter
    { volumeFilterQuery :: Maybe String
    , volumeFilterParty :: Maybe (Id Party)
    , volumeFilterPaginate :: Paginate
    }

instance Semigroup VolumeFilter where
    (<>) (VolumeFilter q1 p1 p) (VolumeFilter q2 p2 _) =
        VolumeFilter (q1 <> q2) (p1 <|> p2) p

instance Monoid VolumeFilter where
    mempty = VolumeFilter Nothing Nothing def
    mappend = (<>)

volumeFilter :: VolumeFilter -> BS.ByteString
volumeFilter VolumeFilter {..} = BS.concat
    [ withq
        volumeFilterParty
        (const " JOIN volume_access ON volume.id = volume_access.volume")
    , withq
        volumeFilterQuery
        (\n ->
            " JOIN volume_text_idx ON volume.id = volume_text_idx.volume, plainto_tsquery('english', "
                <> pgLiteralRep n
                <> ") query"
        )
    , " WHERE volume.id > 0 "
    , withq
        volumeFilterParty
        (\p ->
            " AND volume_access.party = "
                <> pgLiteralRep p
                <> " AND volume_access.individual >= 'EDIT'"
        )
    , withq volumeFilterQuery (const " AND ts @@ query")
    , " ORDER BY "
    , withq volumeFilterQuery (const "ts_rank(ts, query) DESC,")
    , withq volumeFilterParty (const "volume_access.individual DESC,")
    , "volume.id DESC "
    , paginateSQL volumeFilterPaginate
    ]
    where withq v f = maybe BS.empty f v

findVolumes :: (MonadHasIdentity c m, MonadDB c m) => VolumeFilter -> m [Volume]
findVolumes pf = do
    ident <- peek
    dbQuery $ unsafeModifyQuery
        $(selectQuery (selectVolume 'ident) "")
        (<> volumeFilter pf)

updateVolumeIndex :: MonadDB c m => m ()
updateVolumeIndex = dbExecute_ "SELECT volume_text_refresh()"

data VolumePublicAccessLevel
    = PublicAccessFull
    | PublicAccessRestricted
    | PublicAccessNone
    deriving (Eq)

instance Show VolumePublicAccessLevel where
    show PublicAccessFull = "full"
    show PublicAccessRestricted = "restricted"
    show PublicAccessNone = "none"

volumePublicAccessSummary :: [VolumeAccess] -> VolumePublicAccessLevel
volumePublicAccessSummary vas = maybe
    PublicAccessNone
    (\va -> case volumeAccessChildren va of
        PermissionNONE -> PublicAccessNone
        PermissionPUBLIC ->
            case
                    toPolicyDefaulting
                        (volumeAccessShareFull va)
                        PublicNoPolicy
                        PublicRestrictedPolicy
                of
                    PublicRestrictedPolicy -> PublicAccessRestricted
                    PublicNoPolicy -> PublicAccessFull
        _ -> PublicAccessFull
    )
    mPublicAccess
  where
    -- can't use equality on parties because Party is a circular type
    mPublicAccess =
        find (\va -> (getPartyId . volumeAccessParty) va == nobodyId) vas
    nobodyId = getPartyId nobodyParty
