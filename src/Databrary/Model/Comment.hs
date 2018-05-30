{-# LANGUAGE TemplateHaskell, QuasiQuotes, RecordWildCards, OverloadedStrings, DataKinds #-}
module Databrary.Model.Comment
  ( module Databrary.Model.Comment.Types
  , blankComment
  , lookupComment
  , lookupSlotComments
  , lookupVolumeCommentRows
  , addComment
  , commentJSON
  ) where

import Control.Applicative (empty, pure)
import Data.Int (Int64)
import Data.Maybe (listToMaybe)
import Data.Monoid ((<>))
import Database.PostgreSQL.Typed.Query
import Database.PostgreSQL.Typed.Types
import qualified Data.ByteString
import Data.ByteString (ByteString)
import qualified Data.String
import qualified Data.Text as T

import Databrary.Has (peek, view)
import qualified Databrary.JSON as JSON
import Databrary.Service.DB
-- import Databrary.Model.SQL
import Databrary.Model.Id.Types
import Databrary.Model.Party
import Databrary.Model.Party.SQL
import Databrary.Model.Identity
import Databrary.Model.Time
import Databrary.Model.Volume.Types
import Databrary.Model.Volume.SQL
import Databrary.Model.Container
import Databrary.Model.Segment
import Databrary.Model.Slot
import Databrary.Model.Comment.Types
-- import Databrary.Model.Comment.SQL

blankComment :: Account -> Slot -> Comment
blankComment who slot = Comment
  { commentId = error "blankComment"
  , commentWho = who
  , commentSlot = slot
  , commentTime = error "blankComment"
  , commentText = ""
  , commentParents = []
  }

lookupComment :: (MonadDB c m, MonadHasIdentity c m) => Id Comment -> m (Maybe Comment)
lookupComment i = do
  let _tenv_acxwT = unknownPGTypeEnv
  ident <- peek
  -- dbQuery1 $(selectQuery (selectComment 'ident) "$!WHERE comment.id = ${i}")
  mRow <- mapRunPrepQuery1
      ((\ _p_acxwU _p_acxwV _p_acxwW _p_acxwX _p_acxwY ->
                       (Data.String.fromString
                          "SELECT comment.id,comment.segment,comment.time,comment.text,comment.thread,party.id,party.name,party.prename,party.orcid,party.affiliation,party.url,account.email,container.id,container.top,container.name,container.date,slot_release.release,volume.id,volume.name,volume.body,volume.alias,volume.doi,volume_creation(volume.id),volume_owners.owners,volume_permission.permission,volume_permission.share_full FROM comment_thread AS comment JOIN party JOIN account USING (id) ON comment.who = account.id JOIN container LEFT JOIN slot_release ON container.id = slot_release.container AND slot_release.segment = '(,)' JOIN volume LEFT JOIN volume_owners ON volume.id = volume_owners.volume JOIN LATERAL   (VALUES      ( CASE WHEN $1              THEN enum_last(NULL::permission)              ELSE volume_access_check(volume.id, $2) END      , CASE WHEN $3              THEN null              ELSE (select share_full                    from volume_access_view                    where volume = volume.id and party = $4                    limit 1) END )   ) AS volume_permission (permission, share_full) ON volume_permission.permission >= 'PUBLIC'::permission ON container.volume = volume.id ON comment.container = container.id WHERE comment.id = $5",
                       [pgEncodeParameter
                          _tenv_acxwT (PGTypeProxy :: PGTypeName "boolean") _p_acxwU,
                        pgEncodeParameter
                          _tenv_acxwT (PGTypeProxy :: PGTypeName "integer") _p_acxwV,
                        pgEncodeParameter
                          _tenv_acxwT (PGTypeProxy :: PGTypeName "boolean") _p_acxwW,
                        pgEncodeParameter
                          _tenv_acxwT (PGTypeProxy :: PGTypeName "integer") _p_acxwX,
                        pgEncodeParameter
                          _tenv_acxwT (PGTypeProxy :: PGTypeName "integer") _p_acxwY],
                       [pgBinaryColumn _tenv_acxwT (PGTypeProxy :: PGTypeName "integer"),
                        pgBinaryColumn _tenv_acxwT (PGTypeProxy :: PGTypeName "segment"),
                        pgBinaryColumn
                          _tenv_acxwT (PGTypeProxy :: PGTypeName "timestamp with time zone"),
                        pgBinaryColumn _tenv_acxwT (PGTypeProxy :: PGTypeName "text"),
                        pgBinaryColumn _tenv_acxwT (PGTypeProxy :: PGTypeName "integer[]"),
                        pgBinaryColumn _tenv_acxwT (PGTypeProxy :: PGTypeName "integer"),
                        pgBinaryColumn _tenv_acxwT (PGTypeProxy :: PGTypeName "text"),
                        pgBinaryColumn _tenv_acxwT (PGTypeProxy :: PGTypeName "text"),
                        pgBinaryColumn _tenv_acxwT (PGTypeProxy :: PGTypeName "bpchar"),
                        pgBinaryColumn _tenv_acxwT (PGTypeProxy :: PGTypeName "text"),
                        pgBinaryColumn _tenv_acxwT (PGTypeProxy :: PGTypeName "text"),
                        pgBinaryColumn
                          _tenv_acxwT (PGTypeProxy :: PGTypeName "character varying"),
                        pgBinaryColumn _tenv_acxwT (PGTypeProxy :: PGTypeName "integer"),
                        pgBinaryColumn _tenv_acxwT (PGTypeProxy :: PGTypeName "boolean"),
                        pgBinaryColumn _tenv_acxwT (PGTypeProxy :: PGTypeName "text"),
                        pgBinaryColumn _tenv_acxwT (PGTypeProxy :: PGTypeName "date"),
                        pgBinaryColumn _tenv_acxwT (PGTypeProxy :: PGTypeName "release"),
                        pgBinaryColumn _tenv_acxwT (PGTypeProxy :: PGTypeName "integer"),
                        pgBinaryColumn _tenv_acxwT (PGTypeProxy :: PGTypeName "text"),
                        pgBinaryColumn _tenv_acxwT (PGTypeProxy :: PGTypeName "text"),
                        pgBinaryColumn
                          _tenv_acxwT (PGTypeProxy :: PGTypeName "character varying"),
                        pgBinaryColumn
                          _tenv_acxwT (PGTypeProxy :: PGTypeName "character varying"),
                        pgBinaryColumn
                          _tenv_acxwT (PGTypeProxy :: PGTypeName "timestamp with time zone"),
                        pgBinaryColumn _tenv_acxwT (PGTypeProxy :: PGTypeName "text[]"),
                        pgBinaryColumn
                          _tenv_acxwT (PGTypeProxy :: PGTypeName "permission"),
                        pgBinaryColumn _tenv_acxwT (PGTypeProxy :: PGTypeName "boolean")]))
         (identitySuperuser ident)
         (view ident :: Id Party)
         (identitySuperuser ident)
         (view ident :: Id Party)
         i)
               (\
                  [_cid_acxwZ,
                   _csegment_acxx0,
                   _ctime_acxx1,
                   _ctext_acxx2,
                   _cthread_acxx3,
                   _cid_acxx4,
                   _cname_acxx5,
                   _cprename_acxx6,
                   _corcid_acxx7,
                   _caffiliation_acxx8,
                   _curl_acxx9,
                   _cemail_acxxa,
                   _cid_acxxb,
                   _ctop_acxxc,
                   _cname_acxxd,
                   _cdate_acxxe,
                   _crelease_acxxf,
                   _cid_acxxg,
                   _cname_acxxh,
                   _cbody_acxxi,
                   _calias_acxxj,
                   _cdoi_acxxk,
                   _cvolume_creation_acxxl,
                   _cowners_acxxm,
                   _cpermission_acxxn,
                   _cshare_full_acxxo]
                  -> (pgDecodeColumnNotNull
                        _tenv_acxwT (PGTypeProxy :: PGTypeName "integer") _cid_acxwZ, 
                      pgDecodeColumnNotNull
                        _tenv_acxwT (PGTypeProxy :: PGTypeName "segment") _csegment_acxx0, 
                      pgDecodeColumnNotNull
                        _tenv_acxwT
                        (PGTypeProxy :: PGTypeName "timestamp with time zone")
                        _ctime_acxx1, 
                      pgDecodeColumnNotNull
                        _tenv_acxwT (PGTypeProxy :: PGTypeName "text") _ctext_acxx2, 
                      pgDecodeColumnNotNull
                        _tenv_acxwT
                        (PGTypeProxy :: PGTypeName "integer[]")
                        _cthread_acxx3, 
                      pgDecodeColumnNotNull
                        _tenv_acxwT (PGTypeProxy :: PGTypeName "integer") _cid_acxx4, 
                      pgDecodeColumnNotNull
                        _tenv_acxwT (PGTypeProxy :: PGTypeName "text") _cname_acxx5, 
                      pgDecodeColumnNotNull
                        _tenv_acxwT (PGTypeProxy :: PGTypeName "text") _cprename_acxx6, 
                      pgDecodeColumnNotNull
                        _tenv_acxwT (PGTypeProxy :: PGTypeName "bpchar") _corcid_acxx7, 
                      pgDecodeColumnNotNull
                        _tenv_acxwT
                        (PGTypeProxy :: PGTypeName "text")
                        _caffiliation_acxx8, 
                      pgDecodeColumnNotNull
                        _tenv_acxwT (PGTypeProxy :: PGTypeName "text") _curl_acxx9, 
                      pgDecodeColumnNotNull
                        _tenv_acxwT
                        (PGTypeProxy :: PGTypeName "character varying")
                        _cemail_acxxa, 
                      pgDecodeColumnNotNull
                        _tenv_acxwT (PGTypeProxy :: PGTypeName "integer") _cid_acxxb, 
                      pgDecodeColumnNotNull
                        _tenv_acxwT (PGTypeProxy :: PGTypeName "boolean") _ctop_acxxc, 
                      pgDecodeColumnNotNull
                        _tenv_acxwT (PGTypeProxy :: PGTypeName "text") _cname_acxxd, 
                      pgDecodeColumnNotNull
                        _tenv_acxwT (PGTypeProxy :: PGTypeName "date") _cdate_acxxe, 
                      pgDecodeColumnNotNull
                        _tenv_acxwT (PGTypeProxy :: PGTypeName "release") _crelease_acxxf, 
                      pgDecodeColumnNotNull
                        _tenv_acxwT (PGTypeProxy :: PGTypeName "integer") _cid_acxxg, 
                      pgDecodeColumnNotNull
                        _tenv_acxwT (PGTypeProxy :: PGTypeName "text") _cname_acxxh, 
                      pgDecodeColumnNotNull
                        _tenv_acxwT (PGTypeProxy :: PGTypeName "text") _cbody_acxxi, 
                      pgDecodeColumnNotNull
                        _tenv_acxwT
                        (PGTypeProxy :: PGTypeName "character varying")
                        _calias_acxxj, 
                      pgDecodeColumnNotNull
                        _tenv_acxwT
                        (PGTypeProxy :: PGTypeName "character varying")
                        _cdoi_acxxk, 
                      pgDecodeColumnNotNull
                        _tenv_acxwT
                        (PGTypeProxy :: PGTypeName "timestamp with time zone")
                        _cvolume_creation_acxxl, 
                      pgDecodeColumnNotNull
                        _tenv_acxwT (PGTypeProxy :: PGTypeName "text[]") _cowners_acxxm, 
                      pgDecodeColumnNotNull
                        _tenv_acxwT
                        (PGTypeProxy :: PGTypeName "permission")
                        _cpermission_acxxn, 
                      pgDecodeColumnNotNull
                        _tenv_acxwT
                        (PGTypeProxy :: PGTypeName "boolean")
                        _cshare_full_acxxo))
  pure
    (fmap
      (\ (vid_acxwt, vsegment_acxwu, vtime_acxwv, vtext_acxww,
          vthread_acxwx, vid_acxwy, vname_acxwz, vprename_acxwA,
          vorcid_acxwB, vaffiliation_acxwC, vurl_acxwD, vemail_acxwE,
          vid_acxwF, vtop_acxwG, vname_acxwH, vdate_acxwI, vrelease_acxwJ,
          vid_acxwK, vname_acxwL, vbody_acxwM, valias_acxwN, vdoi_acxwO,
          vc_acxwP, vowners_acxwQ, vpermission_acxwR, vfull_acxwS)
         -> ($)
              (($)
                 (makeComment
                    vid_acxwt vsegment_acxwu vtime_acxwv vtext_acxww vthread_acxwx)
                 (Databrary.Model.Party.SQL.permissionParty
                    (Databrary.Model.Party.SQL.makeAccount
                       (PartyRow
                          vid_acxwy
                          vname_acxwz
                          vprename_acxwA
                          vorcid_acxwB
                          vaffiliation_acxwC
                          vurl_acxwD)
                       (Account vemail_acxwE))
                    Nothing
                    ident))
              (($)
                 (Container
                    (ContainerRow vid_acxwF vtop_acxwG vname_acxwH vdate_acxwI)
                    vrelease_acxwJ)
                 (Databrary.Model.Volume.SQL.makeVolume
                    (Databrary.Model.Volume.SQL.setCreation
                       (VolumeRow
                          vid_acxwK vname_acxwL vbody_acxwM valias_acxwN vdoi_acxwO)
                       vc_acxwP)
                    vowners_acxwQ
                    (Databrary.Model.Volume.SQL.makePermInfo
                       vpermission_acxwR vfull_acxwS))))
    mRow)

lookupSlotComments :: (MonadDB c m, MonadHasIdentity c m) => Slot -> Int -> m [Comment]
lookupSlotComments (Slot c s) n = do
  let _tenv_acBuC = unknownPGTypeEnv
  ident <- peek
  -- dbQuery $ ($ c) <$> $(selectQuery (selectContainerComment 'ident) "$!WHERE comment.container = ${containerId $ containerRow c} AND comment.segment && ${s} ORDER BY comment.thread LIMIT ${fromIntegral n :: Int64}")
  rows <- mapRunPrepQuery
      ((\ _p_acBuD _p_acBuE _p_acBuF ->
                       (Data.String.fromString
                          "SELECT comment.id,comment.segment,comment.time,comment.text,comment.thread,party.id,party.name,party.prename,party.orcid,party.affiliation,party.url,account.email FROM comment_thread AS comment JOIN party JOIN account USING (id) ON comment.who = account.id WHERE comment.container = $1 AND comment.segment && $2 ORDER BY comment.thread LIMIT $3",
                       [pgEncodeParameter
                          _tenv_acBuC (PGTypeProxy :: PGTypeName "integer") _p_acBuD,
                        pgEncodeParameter
                          _tenv_acBuC (PGTypeProxy :: PGTypeName "segment") _p_acBuE,
                        pgEncodeParameter
                          _tenv_acBuC (PGTypeProxy :: PGTypeName "bigint") _p_acBuF],
                       [pgBinaryColumn _tenv_acBuC (PGTypeProxy :: PGTypeName "integer"),
                        pgBinaryColumn _tenv_acBuC (PGTypeProxy :: PGTypeName "segment"),
                        pgBinaryColumn
                          _tenv_acBuC (PGTypeProxy :: PGTypeName "timestamp with time zone"),
                        pgBinaryColumn _tenv_acBuC (PGTypeProxy :: PGTypeName "text"),
                        pgBinaryColumn _tenv_acBuC (PGTypeProxy :: PGTypeName "integer[]"),
                        pgBinaryColumn _tenv_acBuC (PGTypeProxy :: PGTypeName "integer"),
                        pgBinaryColumn _tenv_acBuC (PGTypeProxy :: PGTypeName "text"),
                        pgBinaryColumn _tenv_acBuC (PGTypeProxy :: PGTypeName "text"),
                        pgBinaryColumn _tenv_acBuC (PGTypeProxy :: PGTypeName "bpchar"),
                        pgBinaryColumn _tenv_acBuC (PGTypeProxy :: PGTypeName "text"),
                        pgBinaryColumn _tenv_acBuC (PGTypeProxy :: PGTypeName "text"),
                        pgBinaryColumn
                          _tenv_acBuC (PGTypeProxy :: PGTypeName "character varying")]))
         (containerId $ containerRow c) s (fromIntegral n :: Int64))
               (\
                  [_cid_acBuG,
                   _csegment_acBuH,
                   _ctime_acBuI,
                   _ctext_acBuJ,
                   _cthread_acBuK,
                   _cid_acBuL,
                   _cname_acBuM,
                   _cprename_acBuN,
                   _corcid_acBuO,
                   _caffiliation_acBuP,
                   _curl_acBuQ,
                   _cemail_acBuR]
                  -> (pgDecodeColumnNotNull
                        _tenv_acBuC (PGTypeProxy :: PGTypeName "integer") _cid_acBuG, 
                      pgDecodeColumnNotNull
                        _tenv_acBuC (PGTypeProxy :: PGTypeName "segment") _csegment_acBuH, 
                      pgDecodeColumnNotNull
                        _tenv_acBuC
                        (PGTypeProxy :: PGTypeName "timestamp with time zone")
                        _ctime_acBuI, 
                      pgDecodeColumnNotNull
                        _tenv_acBuC (PGTypeProxy :: PGTypeName "text") _ctext_acBuJ, 
                      pgDecodeColumnNotNull
                        _tenv_acBuC
                        (PGTypeProxy :: PGTypeName "integer[]")
                        _cthread_acBuK, 
                      pgDecodeColumnNotNull
                        _tenv_acBuC (PGTypeProxy :: PGTypeName "integer") _cid_acBuL, 
                      pgDecodeColumnNotNull
                        _tenv_acBuC (PGTypeProxy :: PGTypeName "text") _cname_acBuM, 
                      pgDecodeColumnNotNull
                        _tenv_acBuC (PGTypeProxy :: PGTypeName "text") _cprename_acBuN, 
                      pgDecodeColumnNotNull
                        _tenv_acBuC (PGTypeProxy :: PGTypeName "bpchar") _corcid_acBuO, 
                      pgDecodeColumnNotNull
                        _tenv_acBuC
                        (PGTypeProxy :: PGTypeName "text")
                        _caffiliation_acBuP, 
                      pgDecodeColumnNotNull
                        _tenv_acBuC (PGTypeProxy :: PGTypeName "text") _curl_acBuQ, 
                      pgDecodeColumnNotNull
                        _tenv_acBuC
                        (PGTypeProxy :: PGTypeName "character varying")
                        _cemail_acBuR))
  pure
    (fmap
      (\ (vid_acBu6, vsegment_acBu7, vtime_acBu8, vtext_acBu9,
          vthread_acBua, vid_acBub, vname_acBuc, vprename_acBud,
          vorcid_acBue, vaffiliation_acBuf, vurl_acBug, vemail_acBuh)
         -> (makeComment
                 vid_acBu6 vsegment_acBu7 vtime_acBu8 vtext_acBu9 vthread_acBua)
              (Databrary.Model.Party.SQL.permissionParty
                 (Databrary.Model.Party.SQL.makeAccount
                    (PartyRow
                       vid_acBub
                       vname_acBuc
                       vprename_acBud
                       vorcid_acBue
                       vaffiliation_acBuf
                       vurl_acBug)
                    (Account vemail_acBuh))
                 Nothing
                 ident)
              c)
      rows)

mapQuery :: ByteString -> ([PGValue] -> a) -> PGSimpleQuery a
mapQuery qry mkResult =
  fmap mkResult (rawPGSimpleQuery qry)

makeCommentRow :: Id Comment -> Id Container -> Segment -> Id Party -> Timestamp -> T.Text -> CommentRow
makeCommentRow i c s w t x = CommentRow i w (SlotId c s) t x

lookupVolumeCommentRows :: MonadDB c m => Volume -> m [CommentRow]
lookupVolumeCommentRows v = do
  let _tenv_a8I48 = unknownPGTypeEnv
  dbQuery -- .(selectQuery selectCommentRow "JOIN container ON comment.container = container.id WHERE container.volume = ${volumeId $ volumeRow v} ORDER BY container")
   (fmap
      (\ (vid_a8I38, vcontainer_a8I39, vsegment_a8I3a, vwho_a8I3b,
          vtime_a8I3c, vtext_a8I3d)
         -> makeCommentRow
              vid_a8I38
              vcontainer_a8I39
              vsegment_a8I3a
              vwho_a8I3b
              vtime_a8I3c
              vtext_a8I3d)
      (mapQuery
        ((\ _p_a8I49 ->
                       (Data.ByteString.concat
                          [Data.String.fromString
                             "SELECT comment.id,comment.container,comment.segment,comment.who,comment.time,comment.text FROM comment JOIN container ON comment.container = container.id WHERE container.volume = ",
                           Database.PostgreSQL.Typed.Types.pgEscapeParameter
                             _tenv_a8I48
                             (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                                Database.PostgreSQL.Typed.Types.PGTypeName "integer")
                             _p_a8I49,
                           Data.String.fromString " ORDER BY container"]))
         (volumeId $ volumeRow v))
               (\ [_cid_a8I4a,
                   _ccontainer_a8I4b,
                   _csegment_a8I4c,
                   _cwho_a8I4d,
                   _ctime_a8I4e,
                   _ctext_a8I4f]
                  -> (Database.PostgreSQL.Typed.Types.pgDecodeColumnNotNull
                        _tenv_a8I48
                        (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                           Database.PostgreSQL.Typed.Types.PGTypeName "integer")
                        _cid_a8I4a, 
                      Database.PostgreSQL.Typed.Types.pgDecodeColumnNotNull
                        _tenv_a8I48
                        (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                           Database.PostgreSQL.Typed.Types.PGTypeName "integer")
                        _ccontainer_a8I4b, 
                      Database.PostgreSQL.Typed.Types.pgDecodeColumnNotNull
                        _tenv_a8I48
                        (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                           Database.PostgreSQL.Typed.Types.PGTypeName "segment")
                        _csegment_a8I4c, 
                      Database.PostgreSQL.Typed.Types.pgDecodeColumnNotNull
                        _tenv_a8I48
                        (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                           Database.PostgreSQL.Typed.Types.PGTypeName "integer")
                        _cwho_a8I4d, 
                      Database.PostgreSQL.Typed.Types.pgDecodeColumnNotNull
                        _tenv_a8I48
                        (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                           Database.PostgreSQL.Typed.Types.PGTypeName "timestamp with time zone")
                        _ctime_a8I4e, 
                      Database.PostgreSQL.Typed.Types.pgDecodeColumnNotNull
                        _tenv_a8I48
                        (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                           Database.PostgreSQL.Typed.Types.PGTypeName "text")
                        _ctext_a8I4f))))

addComment :: MonadDB c m => Comment -> m Comment
addComment c@Comment{..} = do
  let _tenv_a8Iah = unknownPGTypeEnv
  (i, t) <- dbQuery1' -- [pgSQL|INSERT INTO comment (who, container, segment, text, parent) VALUES (${partyId $ partyRow $ accountParty commentWho}, ${containerId $ containerRow $ slotContainer commentSlot}, ${slotSegment commentSlot}, ${commentText}, ${listToMaybe commentParents}) RETURNING id, time|]
    (mapQuery
      ((\ _p_a8Iai _p_a8Iak _p_a8Ial _p_a8Iam _p_a8Ian ->
                    (Data.ByteString.concat
                       [Data.String.fromString
                          "INSERT INTO comment (who, container, segment, text, parent) VALUES (",
                        Database.PostgreSQL.Typed.Types.pgEscapeParameter
                          _tenv_a8Iah
                          (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                             Database.PostgreSQL.Typed.Types.PGTypeName "integer")
                          _p_a8Iai,
                        Data.String.fromString ", ",
                        Database.PostgreSQL.Typed.Types.pgEscapeParameter
                          _tenv_a8Iah
                          (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                             Database.PostgreSQL.Typed.Types.PGTypeName "integer")
                          _p_a8Iak,
                        Data.String.fromString ", ",
                        Database.PostgreSQL.Typed.Types.pgEscapeParameter
                          _tenv_a8Iah
                          (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                             Database.PostgreSQL.Typed.Types.PGTypeName "segment")
                          _p_a8Ial,
                        Data.String.fromString ", ",
                        Database.PostgreSQL.Typed.Types.pgEscapeParameter
                          _tenv_a8Iah
                          (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                             Database.PostgreSQL.Typed.Types.PGTypeName "text")
                          _p_a8Iam,
                        Data.String.fromString ", ",
                        Database.PostgreSQL.Typed.Types.pgEscapeParameter
                          _tenv_a8Iah
                          (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                             Database.PostgreSQL.Typed.Types.PGTypeName "integer")
                          _p_a8Ian,
                        Data.String.fromString ") RETURNING id, time"]))
       (partyId $ partyRow $ accountParty commentWho)
       (containerId $ containerRow $ slotContainer commentSlot)
       (slotSegment commentSlot)
       commentText
       (listToMaybe commentParents))
          (\ [_cid_a8Iap, _ctime_a8Iaq]
               -> (Database.PostgreSQL.Typed.Types.pgDecodeColumnNotNull
                     _tenv_a8Iah
                     (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                        Database.PostgreSQL.Typed.Types.PGTypeName "integer")
                     _cid_a8Iap, 
                   Database.PostgreSQL.Typed.Types.pgDecodeColumnNotNull
                     _tenv_a8Iah
                     (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                        Database.PostgreSQL.Typed.Types.PGTypeName "timestamp with time zone")
                     _ctime_a8Iaq)))
  return c
    { commentId = i
    , commentTime = t
    }

commentJSON :: JSON.ToNestedObject o u => Comment -> JSON.Record (Id Comment) o
commentJSON Comment{ commentSlot = Slot{..}, ..} = JSON.Record commentId $
     "container" JSON..=: containerJSON False slotContainer -- should compute based on volume
  <> segmentJSON slotSegment
  <> "who" JSON..=: partyJSON (accountParty commentWho)
  <> "time" JSON..= commentTime
  <> "text" JSON..= commentText
  <> "parents" `JSON.kvObjectOrEmpty` (if null commentParents then empty else pure commentParents)
   
