{-# LANGUAGE TemplateHaskell, ScopedTypeVariables, DataKinds #-}
module Model.Citation
  ( module Model.Citation.Types
  , lookupVolumeCitation
  , lookupVolumesCitations
  , changeVolumeCitation
  , lookupVolumeLinks
  , changeVolumeLinks
  ) where

import Has (peek, view)
import Service.DB
import Model.SQL
import Model.Audit
import Model.Id.Types
import Model.Identity.Types
import Model.Party.Types
import Model.Volume.Types
import Model.Citation.Types
import Model.Citation.SQL

lookupVolumeCitation :: (MonadDB c m) => Volume -> m (Maybe Citation)
lookupVolumeCitation vol =
  dbQuery1 $ fmap ($ Just (volumeName $ volumeRow vol)) $(selectQuery selectVolumeCitation "$WHERE volume_citation.volume = ${volumeId $ volumeRow vol}")

lookupVolumesCitations :: (MonadDB c m, MonadHasIdentity c m) => m [(Volume, Maybe Citation)]
lookupVolumesCitations = do
  ident :: Identity <- peek
  dbQuery $(selectQuery (selectCitation 'ident) "WHERE volume.id > 0")

lookupVolumeLinks :: (MonadDB c m) => Volume -> m [Citation]
lookupVolumeLinks vol =
  dbQuery $(selectQuery selectVolumeLink "$WHERE volume_link.volume = ${volumeId $ volumeRow vol}")

changeVolumeCitation :: (MonadAudit c m) => Volume -> Maybe Citation -> m Bool
changeVolumeCitation vol citem = do
  ident <- getAuditIdentity
  (0 <) <$> maybe
    (dbExecute $(deleteVolumeCitation 'ident 'vol))
    (\cite -> fst <$> updateOrInsert
      $(updateVolumeCitation 'ident 'vol 'cite)
      $(insertVolumeCitation 'ident 'vol 'cite))
    citem

changeVolumeLinks :: (MonadAudit c m) => Volume -> [Citation] -> m ()
changeVolumeLinks vol links = do
  ident <- getAuditIdentity
  dbTransaction $ do
    _ <- dbExecute $(deleteVolumeLink 'ident 'vol)
    mapM_ (\link -> dbExecute $(insertVolumeLink 'ident 'vol 'link)) links
