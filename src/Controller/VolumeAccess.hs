{-# LANGUAGE OverloadedStrings #-}
module Controller.VolumeAccess
  (--  viewVolumeAccess
    postVolumeAccess
  ) where

-- import Control.Monad.IO.Class (liftIO)
import Control.Monad (when, forM_)
import Data.Function (on)
import Data.Int (Int16)

-- import Ops
import Has
import qualified JSON as JSON
import Model.Id
import Model.Permission
import Model.Identity
import Model.Volume hiding (getVolume)
import Model.VolumeAccess
import Model.Party
import Model.Notification.Types
import HTTP.Form (FormDatum(..))
import HTTP.Form.Deform
import HTTP.Path.Parser
import Action
import Controller.Paths
import Controller.Form
import Controller.Volume
import Controller.Notification
-- import View.VolumeAccess
import View.Form (FormHtml)

{- obsolete
viewVolumeAccess :: ActionRoute (Id Volume, VolumeAccessTarget)
viewVolumeAccess = action GET (pathHTML >/> pathId </> pathVolumeAccessTarget) $ \(vi, VolumeAccessTarget ap) -> withAuth $ do
  v <- getVolume PermissionADMIN vi
  a <- maybeAction =<< lookupVolumeAccessParty v ap
  peeks $ blankForm . htmlVolumeAccessForm a
-}

data ManageVolumeAccessRequest =
      DeleteVolumeAccessRequest Bool
    | CreateOrUpdateVolumeAccessRequest Permission Permission (Maybe Int16) (Maybe Bool)

postVolumeAccess :: ActionRoute (Id Volume, VolumeAccessTarget)
postVolumeAccess = action POST (pathJSON >/> pathId </> pathVolumeAccessTarget) $ \(vi, VolumeAccessTarget ap) -> withAuth $ do
  v <- getVolume (if ap == partyId (partyRow staffParty) then PermissionEDIT else PermissionADMIN) vi
  a <- maybeAction =<< lookupVolumeAccessParty v ap
  u <- peek
  let su = identityAdmin u
      ru = unId ap > 0
  a' <- runForm (Nothing :: Maybe (RequestContext -> FormHtml a)) $ do
    csrfForm
    DeleteVolumeAccessRequest delete <- DeleteVolumeAccessRequest <$> ("delete" .:> deform)
    let del
          | delete = return PermissionNONE
          | otherwise = deform
    individual <- "individual" .:> (del
      >>= deformCheck "Cannot share full access." ((||) ru . (PermissionSHARED >=))
      >>= deformCheck "Cannot remove your ownership." ((||) (su || not (volumeAccessProvidesADMIN a)) . (PermissionADMIN <=)))
    children <- "children" .:> (del
      >>= deformCheck "Inherited access must not exceed individual." (individual >=)
      >>= deformCheck "You are not authorized to share data." ((||) (ru || accessSite u >= PermissionEDIT) . (PermissionNONE ==)))
    sort <- "sort" .:> deformNonEmpty deform
    mShareFull <-
      if (ap, individual) `elem` [(getPartyId nobodyParty, PermissionPUBLIC), (getPartyId rootParty, PermissionSHARED)]
      then do
        _ <- "share_full" .:> (deformCheck "Required" (not . (== FormDatumNone)) =<< deform) -- convulated way of requiring
        Just <$> ("share_full" .:> deform)
      else pure Nothing
    let _ = CreateOrUpdateVolumeAccessRequest individual children sort mShareFull
    return a
      { volumeAccessIndividual = individual
      , volumeAccessChildren = children
      , volumeAccessSort = sort
      , volumeAccessShareFull = mShareFull
      }
  -- liftIO $ print ("vol access full", volumeAccessShareFull a')
  r <- changeVolumeAccess a'
  if ap == partyId (partyRow rootParty) && on (/=) volumeAccessChildren a' a
    then do
      createVolumeNotification v $ \n -> (n NoticeVolumeSharing)
        { notificationPermission = Just $ volumeAccessChildren a'
        }
      broadcastNotification (volumeAccessChildren a' >= PermissionSHARED) $ \n -> (n NoticeSharedVolume)
        { notificationVolume = Just $ volumeRow v
        }
    else when (ru && on (/=) volumeAccessIndividual a' a) $ do
      createVolumeNotification v $ \n -> (n NoticeVolumeAccessOther)
        { notificationParty = Just $ partyRow $ volumeAccessParty a'
        , notificationPermission = Just $ volumeAccessIndividual a'
        }
      when (ap /= view u) $ forM_ (partyAccount (volumeAccessParty a')) $ \t ->
        createNotification (blankNotification t NoticeVolumeAccess)
          { notificationVolume = Just $ volumeRow v
          , notificationPermission = Just $ volumeAccessIndividual a'
          }
  return $ okResponse [] $ JSON.pairs $ volumeAccessPartyJSON (if r then a' else a)
  -- HTML -> peeks $ otherRouteResponse [] viewVolumeAccess arg
