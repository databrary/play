{-# LANGUAGE TemplateHaskell, QuasiQuotes, RecordWildCards, OverloadedStrings, DataKinds #-}
module Model.Tag
  ( module Model.Tag.Types
  , lookupTag
  , lookupTags
  , findTags
  , addTag
  , lookupVolumeTagUseRows
  , addTagUse
  , removeTagUse
  , lookupTopTagWeight
  , lookupTagCoverage
  , lookupSlotTagCoverage
  , lookupSlotKeywords
  , tagWeightJSON
  , tagCoverageJSON
  ) where

import Control.Applicative (empty, pure)
import Control.Monad (guard)
import qualified Data.ByteString.Char8 as BSC
import Data.Int (Int64)
import Data.Maybe (fromMaybe)
import Data.Monoid ((<>))
import qualified Data.String
-- import Database.PostgreSQL.Typed (pgSQL)
import Database.PostgreSQL.Typed.Types

import Has (peek)
import qualified JSON as JSON
import Service.DB
import Model.SQL
import Model.Party.Types
import Model.Identity.Types
import Model.Volume.Types
import Model.Container.Types
import Model.Slot.Types
import Model.Tag.Types
import Model.Tag.SQL

lookupTag :: MonadDB c m => TagName -> m (Maybe Tag)
lookupTag n =
  dbQuery1 $(selectQuery selectTag "$WHERE tag.name = ${n}::varchar")

lookupTags :: MonadDB c m => m [Tag]
lookupTags = do
  let _tenv_a6Dq8 = unknownPGTypeEnv
  rows <- dbQuery -- (selectQuery selectTag "")
    (mapQuery2
                      (BSC.concat
                         [Data.String.fromString "SELECT tag.id,tag.name FROM tag "])
              (\ [_cid_a6Dq9, _cname_a6Dqa]
                 -> (Database.PostgreSQL.Typed.Types.pgDecodeColumnNotNull
                       _tenv_a6Dq8
                       (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                          Database.PostgreSQL.Typed.Types.PGTypeName "integer")
                       _cid_a6Dq9, 
                     Database.PostgreSQL.Typed.Types.pgDecodeColumnNotNull
                       _tenv_a6Dq8
                       (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                          Database.PostgreSQL.Typed.Types.PGTypeName "character varying")
                       _cname_a6Dqa)))
  pure
    (fmap
      (\ (vid_a6Dpn, vname_a6Dpo) -> Tag vid_a6Dpn vname_a6Dpo)
      rows)

findTags :: MonadDB c m => TagName -> Int -> m [Tag]
findTags (TagName n) lim = -- TagName restrictions obviate pattern escaping
  dbQuery $(selectQuery selectTag "$WHERE tag.name LIKE ${n `BSC.snoc` '%'}::varchar LIMIT ${fromIntegral lim :: Int64}")

addTag :: MonadDB c m => TagName -> m Tag
addTag n = do
  let _tenv_a6GtM = unknownPGTypeEnv
  row <- dbQuery1' -- [pgSQL|!SELECT get_tag(${n})|]
    (mapQuery2
      ((\ _p_a6GtN ->
                    (BSC.concat
                       [Data.String.fromString "SELECT get_tag(",
                        Database.PostgreSQL.Typed.Types.pgEscapeParameter
                          _tenv_a6GtM
                          (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                             Database.PostgreSQL.Typed.Types.PGTypeName "character varying")
                          _p_a6GtN,
                        Data.String.fromString ")"]))
      n)
      (\ [_cget_tag_a6GtO]
               -> (Database.PostgreSQL.Typed.Types.pgDecodeColumnNotNull
                     _tenv_a6GtM
                     (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                        Database.PostgreSQL.Typed.Types.PGTypeName "integer")
                     _cget_tag_a6GtO)))
  pure ((`Tag` n) row)

lookupVolumeTagUseRows :: MonadDB c m => Volume -> m [TagUseRow]
lookupVolumeTagUseRows v = do
  let _tenv_a6PCr = unknownPGTypeEnv
  rows <- dbQuery -- (selectQuery selectTagUseRow "JOIN container ON tag_use.container = container.id WHERE container.volume = ${volumeId $ volumeRow v} ORDER BY container.id")
   (mapQuery2
      ((\ _p_a6PCs ->
                       (BSC.concat
                          [Data.String.fromString
                             "SELECT tag_use.who,tag_use.container,tag_use.segment,tag_use.tableoid = 'keyword_use'::regclass,tag.id,tag.name FROM tag_use JOIN tag ON tag_use.tag = tag.id JOIN container ON tag_use.container = container.id WHERE container.volume = ",
                           Database.PostgreSQL.Typed.Types.pgEscapeParameter
                             _tenv_a6PCr
                             (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                                Database.PostgreSQL.Typed.Types.PGTypeName "integer")
                             _p_a6PCs,
                           Data.String.fromString " ORDER BY container.id"]))
         (volumeId $ volumeRow v))
               (\ 
                  [_cwho_a6PCt,
                   _ccontainer_a6PCu,
                   _csegment_a6PCv,
                   _ccolumn_a6PCw,
                   _cid_a6PCx,
                   _cname_a6PCy]
                  -> (Database.PostgreSQL.Typed.Types.pgDecodeColumnNotNull
                        _tenv_a6PCr
                        (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                           Database.PostgreSQL.Typed.Types.PGTypeName "integer")
                        _cwho_a6PCt, 
                      Database.PostgreSQL.Typed.Types.pgDecodeColumnNotNull
                        _tenv_a6PCr
                        (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                           Database.PostgreSQL.Typed.Types.PGTypeName "integer")
                        _ccontainer_a6PCu, 
                      Database.PostgreSQL.Typed.Types.pgDecodeColumnNotNull
                        _tenv_a6PCr
                        (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                           Database.PostgreSQL.Typed.Types.PGTypeName "segment")
                        _csegment_a6PCv, 
                      Database.PostgreSQL.Typed.Types.pgDecodeColumn
                        _tenv_a6PCr
                        (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                           Database.PostgreSQL.Typed.Types.PGTypeName "boolean")
                        _ccolumn_a6PCw, 
                      Database.PostgreSQL.Typed.Types.pgDecodeColumnNotNull
                        _tenv_a6PCr
                        (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                           Database.PostgreSQL.Typed.Types.PGTypeName "integer")
                        _cid_a6PCx, 
                      Database.PostgreSQL.Typed.Types.pgDecodeColumnNotNull
                        _tenv_a6PCr
                        (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                           Database.PostgreSQL.Typed.Types.PGTypeName "character varying")
                        _cname_a6PCy)))
  pure
    (fmap
      (\ (vwho_a6PC1, vcontainer_a6PC2, vsegment_a6PC3, vregclass_a6PC4,
          vid_a6PC5, vname_a6PC6)
         -> ($)
              (($)
                 (Model.Tag.SQL.makeTagUseRow
                    vwho_a6PC1 vcontainer_a6PC2 vsegment_a6PC3)
                 vregclass_a6PC4)
              (Tag vid_a6PC5 vname_a6PC6))
      rows)
     

addTagUse :: MonadDB c m => TagUse -> m Bool
addTagUse t = either (const False) id <$> do
  let (_tenv_a6PDJ, _tenv_a6PEH) = (unknownPGTypeEnv, unknownPGTypeEnv)
  dbTryJust (guard . isExclusionViolation)
    $ dbExecute1 (if tagKeyword t
      then -- (insertTagUse True 't)
       (mapQuery2
         ((\ _p_a6PDK _p_a6PDL _p_a6PDM _p_a6PDN ->
                         (BSC.concat
                            [Data.String.fromString
                               "INSERT INTO keyword_use (tag, container, segment, who) VALUES (",
                             Database.PostgreSQL.Typed.Types.pgEscapeParameter
                               _tenv_a6PDJ
                               (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                                  Database.PostgreSQL.Typed.Types.PGTypeName "integer")
                               _p_a6PDK,
                             Data.String.fromString ", ",
                             Database.PostgreSQL.Typed.Types.pgEscapeParameter
                               _tenv_a6PDJ
                               (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                                  Database.PostgreSQL.Typed.Types.PGTypeName "integer")
                               _p_a6PDL,
                             Data.String.fromString ", ",
                             Database.PostgreSQL.Typed.Types.pgEscapeParameter
                               _tenv_a6PDJ
                               (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                                  Database.PostgreSQL.Typed.Types.PGTypeName "segment")
                               _p_a6PDM,
                             Data.String.fromString ", ",
                             Database.PostgreSQL.Typed.Types.pgEscapeParameter
                               _tenv_a6PDJ
                               (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                                  Database.PostgreSQL.Typed.Types.PGTypeName "integer")
                               _p_a6PDN,
                             Data.String.fromString ")"]))
           (tagId $ useTag t)
           (containerId $ containerRow $ slotContainer $ tagSlot t)
           (slotSegment $ tagSlot t)
           (partyId $ partyRow $ accountParty $ tagWho t))
         (\[] -> ()))
      else -- (insertTagUse False 't))
       (mapQuery2
         ((\ _p_a6PEI _p_a6PEJ _p_a6PEK _p_a6PEL ->
                    (BSC.concat
                       [Data.String.fromString
                          "INSERT INTO tag_use (tag, container, segment, who) VALUES (",
                        Database.PostgreSQL.Typed.Types.pgEscapeParameter
                          _tenv_a6PEH
                          (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                             Database.PostgreSQL.Typed.Types.PGTypeName "integer")
                          _p_a6PEI,
                        Data.String.fromString ", ",
                        Database.PostgreSQL.Typed.Types.pgEscapeParameter
                          _tenv_a6PEH
                          (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                             Database.PostgreSQL.Typed.Types.PGTypeName "integer")
                          _p_a6PEJ,
                        Data.String.fromString ", ",
                        Database.PostgreSQL.Typed.Types.pgEscapeParameter
                          _tenv_a6PEH
                          (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                             Database.PostgreSQL.Typed.Types.PGTypeName "segment")
                          _p_a6PEK,
                        Data.String.fromString ", ",
                        Database.PostgreSQL.Typed.Types.pgEscapeParameter
                          _tenv_a6PEH
                          (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                             Database.PostgreSQL.Typed.Types.PGTypeName "integer")
                          _p_a6PEL,
                        Data.String.fromString ")"]))
           (tagId $ useTag t)
           (containerId $ containerRow $ slotContainer $ tagSlot t)
           (slotSegment $ tagSlot t)
           (partyId $ partyRow $ accountParty $ tagWho t))
          (\[] -> ())))

removeTagUse :: MonadDB c m => TagUse -> m Int
removeTagUse t = do
  let (_tenv_a6PFr, _tenv_a6PGB) = (unknownPGTypeEnv, unknownPGTypeEnv)
  dbExecute
    (if tagKeyword t
      then -- (deleteTagUse True 't)
       (mapQuery2
          ((\ _p_a6PFs _p_a6PFt _p_a6PFu ->
                    (BSC.concat
                       [Data.String.fromString
                          "DELETE FROM ONLY keyword_use WHERE tag = ",
                        Database.PostgreSQL.Typed.Types.pgEscapeParameter
                          _tenv_a6PFr
                          (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                             Database.PostgreSQL.Typed.Types.PGTypeName "integer")
                          _p_a6PFs,
                        Data.String.fromString " AND container = ",
                        Database.PostgreSQL.Typed.Types.pgEscapeParameter
                          _tenv_a6PFr
                          (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                             Database.PostgreSQL.Typed.Types.PGTypeName "integer")
                          _p_a6PFt,
                        Data.String.fromString " AND segment <@ ",
                        Database.PostgreSQL.Typed.Types.pgEscapeParameter
                          _tenv_a6PFr
                          (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                             Database.PostgreSQL.Typed.Types.PGTypeName "segment")
                          _p_a6PFu]))
            (tagId $ useTag t)
            (containerId $ containerRow $ slotContainer $ tagSlot t)
            (slotSegment $ tagSlot t))
          (\[] -> ()))
      else -- (deleteTagUse False 't))
       (mapQuery2
         ((\ _p_a6PGC _p_a6PGD _p_a6PGE _p_a6PGF ->
                    (BSC.concat
                       [Data.String.fromString "DELETE FROM ONLY tag_use WHERE tag = ",
                        Database.PostgreSQL.Typed.Types.pgEscapeParameter
                          _tenv_a6PGB
                          (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                             Database.PostgreSQL.Typed.Types.PGTypeName "integer")
                          _p_a6PGC,
                        Data.String.fromString " AND container = ",
                        Database.PostgreSQL.Typed.Types.pgEscapeParameter
                          _tenv_a6PGB
                          (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                             Database.PostgreSQL.Typed.Types.PGTypeName "integer")
                          _p_a6PGD,
                        Data.String.fromString " AND segment <@ ",
                        Database.PostgreSQL.Typed.Types.pgEscapeParameter
                          _tenv_a6PGB
                          (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                             Database.PostgreSQL.Typed.Types.PGTypeName "segment")
                          _p_a6PGE,
                        Data.String.fromString " AND who = ",
                        Database.PostgreSQL.Typed.Types.pgEscapeParameter
                          _tenv_a6PGB
                          (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                             Database.PostgreSQL.Typed.Types.PGTypeName "integer")
                          _p_a6PGF]))
           (tagId $ useTag t)
           (containerId $ containerRow $ slotContainer $ tagSlot t)
           (slotSegment $ tagSlot t)
           (partyId $ partyRow $ accountParty $ tagWho t))
          (\[] -> ())))

lookupTopTagWeight :: MonadDB c m => Int -> m [TagWeight]
lookupTopTagWeight lim =
  dbQuery $(selectQuery (selectTagWeight "") "$!ORDER BY weight DESC LIMIT ${fromIntegral lim :: Int64}")

emptyTagCoverage :: Tag -> Container -> TagCoverage
emptyTagCoverage t c = TagCoverage (TagWeight t 0) c [] [] []

lookupTagCoverage :: (MonadDB c m, MonadHasIdentity c m) => Tag -> Slot -> m TagCoverage
lookupTagCoverage t (Slot c s) = do
  ident <- peek
  fromMaybe (emptyTagCoverage t c) <$> dbQuery1 (($ c) . ($ t) <$> $(selectQuery (selectTagCoverage 'ident "WHERE container = ${containerId $ containerRow c} AND segment && ${s} AND tag = ${tagId t}") "$!"))

lookupSlotTagCoverage :: (MonadDB c m, MonadHasIdentity c m) => Slot -> Int -> m [TagCoverage]
lookupSlotTagCoverage slot lim = do
  ident <- peek
  dbQuery $(selectQuery (selectSlotTagCoverage 'ident 'slot) "$!ORDER BY weight DESC LIMIT ${fromIntegral lim :: Int64}")

lookupSlotKeywords :: (MonadDB c m) => Slot -> m [Tag]
lookupSlotKeywords Slot{..} = do
  let _tenv_a6Q2M = unknownPGTypeEnv
  rows <- dbQuery -- (selectQuery selectTag "JOIN keyword_use ON id = tag WHERE container = ${containerId $ containerRow slotContainer} AND segment = ${slotSegment}")
    (mapQuery2
      ((\ _p_a6Q2N _p_a6Q2O ->
                       (BSC.concat
                          [Data.String.fromString
                             "SELECT tag.id,tag.name FROM tag JOIN keyword_use ON id = tag WHERE container = ",
                           Database.PostgreSQL.Typed.Types.pgEscapeParameter
                             _tenv_a6Q2M
                             (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                                Database.PostgreSQL.Typed.Types.PGTypeName "integer")
                             _p_a6Q2N,
                           Data.String.fromString " AND segment = ",
                           Database.PostgreSQL.Typed.Types.pgEscapeParameter
                             _tenv_a6Q2M
                             (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                                Database.PostgreSQL.Typed.Types.PGTypeName "segment")
                             _p_a6Q2O]))
         (containerId $ containerRow slotContainer) slotSegment)
               (\ [_cid_a6Q2P, _cname_a6Q2Q]
                  -> (Database.PostgreSQL.Typed.Types.pgDecodeColumnNotNull
                        _tenv_a6Q2M
                        (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                           Database.PostgreSQL.Typed.Types.PGTypeName "integer")
                        _cid_a6Q2P, 
                      Database.PostgreSQL.Typed.Types.pgDecodeColumnNotNull
                        _tenv_a6Q2M
                        (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                           Database.PostgreSQL.Typed.Types.PGTypeName "character varying")
                        _cname_a6Q2Q)))
  pure
    (fmap
      (\ (vid_a6Q1R, vname_a6Q1S) -> Tag vid_a6Q1R vname_a6Q1S)
      rows)

tagWeightJSON :: JSON.ToObject o => TagWeight -> JSON.Record TagName o
tagWeightJSON TagWeight{..} = JSON.Record (tagName tagWeightTag) $
  "weight" JSON..= tagWeightWeight

tagCoverageJSON :: JSON.ToObject o => TagCoverage -> JSON.Record TagName o
tagCoverageJSON TagCoverage{..} = tagWeightJSON tagCoverageWeight `JSON.foldObjectIntoRec`
 (   "coverage" JSON..= tagCoverageSegments
  <> "keyword" `JSON.kvObjectOrEmpty` (if null tagCoverageKeywords then empty else pure tagCoverageKeywords)
  <> "vote"    `JSON.kvObjectOrEmpty` (if null tagCoverageVotes then empty else pure tagCoverageVotes))
