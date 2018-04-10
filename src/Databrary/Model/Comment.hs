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
import Database.PostgreSQL.Typed (pgSQL)
import Database.PostgreSQL.Typed
import Database.PostgreSQL.Typed.Query
import Database.PostgreSQL.Typed.Types
import qualified Database.PostgreSQL.Typed.Query
import qualified Database.PostgreSQL.Typed.Types
import qualified Data.ByteString
import Data.ByteString (ByteString)
import qualified Data.String
import qualified Data.Text as T

import Databrary.Ops
import Databrary.Has (peek, view)
import qualified Databrary.JSON as JSON
import Databrary.Service.DB
import Databrary.Model.SQL
import Databrary.Model.Id.Types
import Databrary.Model.Party
import Databrary.Model.Identity
import Databrary.Model.Time
import Databrary.Model.Volume.Types
import Databrary.Model.Container
import Databrary.Model.Segment
import Databrary.Model.Slot
import Databrary.Model.Comment.Types
import Databrary.Model.Comment.SQL

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
  ident <- peek
  dbQuery1 $(selectQuery (selectComment 'ident) "$!WHERE comment.id = ${i}")

lookupSlotComments :: (MonadDB c m, MonadHasIdentity c m) => Slot -> Int -> m [Comment]
lookupSlotComments (Slot c s) n = do
  ident <- peek
  dbQuery $ ($ c) <$> $(selectQuery (selectContainerComment 'ident) "$!WHERE comment.container = ${containerId $ containerRow c} AND comment.segment && ${s} ORDER BY comment.thread LIMIT ${fromIntegral n :: Int64}")

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
  <> "parents" JSON..=? (if null commentParents then empty else pure commentParents)
   
