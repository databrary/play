{-# LANGUAGE RecordWildCards, TemplateHaskell, QuasiQuotes, DataKinds #-}
module EZID.Volume
  ( updateEZID
  ) where

import Control.Arrow ((&&&))
import Control.Monad ((<=<))
import Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import Data.Function (on)
import Data.List (deleteFirstsBy)
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Time.Clock (utctDay, getCurrentTime)
import Database.PostgreSQL.Typed.Query (pgSQL)
import qualified Network.Wai as Wai

import Has
import Service.DB
import Service.Log
import Context
import Model.Time
import Model.Permission
import Model.Identity
import Model.Id
import Model.Volume
import Model.VolumeAccess
import Model.Citation
import Model.Container
import Model.Slot
import Model.Funding
import Model.Tag
import Action.Route
import Controller.Volume
import EZID.API
import EZID.DataCite

useTDB

volumeDOISuffix :: Id Volume -> BS.ByteString
volumeDOISuffix i = BSC.pack $ '.' : show i

volumeEZID :: (MonadDB c m, MonadHasIdentity c m) => Volume -> Maybe Citation -> m EZIDMeta
volumeEZID v@Volume{ volumeRow = VolumeRow{..}, ..} cite = do
  top <- lookupVolumeTopContainer v
  own <- lookupVolumeAccess v PermissionADMIN
  fund <- lookupVolumeFunding v
  link <- lookupVolumeLinks v
  key <- lookupSlotKeywords (containerSlot top)
  return EZIDPublic
    { ezidTarget = actionURI (Just Wai.defaultRequest) viewVolume (HTML, volumeId) []
    , ezidDataCite = DataCite
      { dataCiteDOI = volumeDOI
      , dataCiteTitle = volumeName
      , dataCiteAuthors = map volumeAccessParty own
      , dataCiteYear = dateYear (utctDay volumeCreation)
      , dataCiteDescription = volumeBody
      , dataCiteFunders = fund
      , dataCitePublication = citationURL =<< cite
      , dataCiteReferences = mapMaybe citationURL link
      , dataCiteSubjects = map (tagNameBS . tagName) key
      }
    }

lookupVolumeDOIs :: MonadDB c m => m [(Id Volume, BS.ByteString)]
lookupVolumeDOIs = dbQuery [pgSQL|!SELECT id, doi FROM volume WHERE doi IS NOT NULL|]

addVolumeDOI :: MonadDB c m => Id Volume -> BS.ByteString -> m Bool
addVolumeDOI v d = dbExecute1 [pgSQL|UPDATE volume SET doi = ${d} WHERE id = ${v} AND doi IS NULL|]

updateVolume :: Volume -> Maybe Citation -> EZIDM Bool
updateVolume v = maybe
  (maybe (return False) (addVolumeDOI $ volumeId $ volumeRow v) <=< ezidCreate (volumeDOISuffix $ volumeId $ volumeRow v))
  ezidModify
  (volumeDOI $ volumeRow v)
  <=< volumeEZID v

removeVolume :: Id Volume -> BS.ByteString -> EZIDM Bool
removeVolume _ d = ezidModify d EZIDUnavailable

updateEZID :: BackgroundContextM (Maybe Bool)
updateEZID = runEZIDM $ do
  r <- ezidStatus
  if r
    then do
      vl <- lookupVolumesCitations
      mapM_ (uncurry updateVolume) vl
      dl <- lookupVolumeDOIs
      mapM_ (uncurry removeVolume) $
        deleteFirstsBy (on (==) fst) dl (map ((volumeId &&& fromMaybe BS.empty . volumeDOI) . volumeRow . fst) vl)
    else do
      t <- liftIO getCurrentTime
      focusIO $ logMsg t "ezid is down"
  return r
