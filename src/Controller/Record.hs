{-# LANGUAGE OverloadedStrings #-}
module Controller.Record
  ( getRecord
  , viewRecord
  , createRecord
  , postRecordMeasure
  , deleteRecord
  , postRecordSlot
  , deleteRecordSlot
  , deleteRecordAllSlot
  ) where

import Control.Monad (when, unless)
import Control.Monad.Trans.Class (lift)
import qualified Data.ByteString as BS
import Data.Maybe (isNothing, fromMaybe)
import qualified Data.Text as T
import Network.HTTP.Types (noContent204, conflict409)

import qualified JSON
import Action.Route
import Action.Response
import Action
import Model.Id
import Model.Volume
import Model.Permission hiding (checkPermission)
import Model.Record
import Model.Category
import Model.RecordSlot
import Model.Metric
import Model.Measure
import Model.Segment
import Model.Slot
import HTTP.Form.Deform
import HTTP.Path.Parser
import Controller.Paths
import Controller.Form
import Controller.Volume
import Controller.Slot
import Controller.Permission
import View.Form (FormHtml)

getRecord :: Permission -> Id Record -> Handler Record
getRecord p i =
  checkPermissionOld p =<< maybeAction =<< lookupRecord i

viewRecord :: ActionRoute (Id Record)
viewRecord = action GET (pathJSON >/> pathId) $ \i -> withAuth $ do
  rec <- getRecord PermissionPUBLIC i
  let v = recordVolume rec
  _ <- maybeAction (if volumeIsPublicRestricted v then Nothing else Just ()) -- block if restricted
  return $ okResponse [] $ JSON.recordEncoding $ recordJSON False rec -- json should consult volume

data CreateRecordRequest = CreateRecordRequest Category

createRecord :: ActionRoute (Id Volume)
createRecord = action POST (pathJSON >/> pathId </< "record") $ \vi -> withAuth $ do
  vol <- getVolume PermissionEDIT vi
  br <- runForm (Nothing :: Maybe (RequestContext -> FormHtml a)) $ do
    csrfForm
    CreateRecordRequest cat <- CreateRecordRequest <$> ("category" .:> (deformMaybe' "No such category" . getCategory =<< deform))
    return $ blankRecord cat vol
  rec <- addRecord br
  return $ okResponse [] $ JSON.recordEncoding $ recordJSON False rec -- recordJSON not restricted because EDIT

data ManageRecordMeasureRequest = ManageRecordMeasureRequest (Maybe BS.ByteString)

postRecordMeasure :: ActionRoute (Id Record, Id Metric)
postRecordMeasure = action POST (pathJSON >/> pathId </> pathId) $ \(ri, mi) -> withAuth $ do
  record <- getRecord PermissionEDIT ri
  met <- maybeAction $ getMetric mi
  let mkMeasure datum = Measure record met datum
  rec' <- runForm (Nothing :: Maybe (RequestContext -> FormHtml a)) $ do
    csrfForm
    ManageRecordMeasureRequest mDatum <- ManageRecordMeasureRequest <$> deformSync' ("datum" .:> deformNonEmpty deform)
    maybe
      (lift $ removeRecordMeasure $ mkMeasure "") -- delete measure data
      (\d -> do  -- add or update measure data
        mRecord <- lift $ changeRecordMeasure $ mkMeasure d
        when (isNothing mRecord) $
            deformError $
                T.pack $
                       "Invalid "
                    ++ show (metricType met)
                    ++ (if metricType met == MeasureTypeDate then " (please use YYYY-MM-DD)" else "")
        return $ fromMaybe record mRecord)
      mDatum
  return $ okResponse [] $ JSON.recordEncoding $ recordJSON False rec' -- recordJSON not restricted because EDIT

deleteRecord :: ActionRoute (Id Record)
deleteRecord = action DELETE (pathJSON >/> pathId) $ \ri -> withAuth $ do
  guardVerfHeader
  rec <- getRecord PermissionEDIT ri
  r <- removeRecord rec
  unless r $ result $
    response conflict409 [] $ JSON.recordEncoding $ recordJSON False rec -- json not restricted because edit
  return $ emptyResponse noContent204 []

data UpdateRecordSlotRequest = UpdateRecordSlotRequest (Maybe Segment)

postRecordSlot :: ActionRoute (Id Slot, Id Record)
postRecordSlot = action POST (pathJSON >/> pathSlotId </> pathId) $ \(si, ri) -> withAuth $ do
  slot <- getSlot PermissionEDIT si
  rec <- getRecord PermissionEDIT ri
  UpdateRecordSlotRequest src <- runForm Nothing $ do
    csrfForm
    UpdateRecordSlotRequest <$> ("src" .:> deformNonEmpty deform)
  r <- moveRecordSlot (RecordSlot rec slot{ slotSegment = fromMaybe emptySegment src }) (slotSegment slot)
  if r
  then return $ okResponse [] $ JSON.recordEncoding $ recordSlotJSON False (RecordSlot rec slot)
  else return $ okResponse [] $ JSON.recordEncoding $ recordJSON False rec -- recordJSON not restricted because EDIT

deleteRecordSlot :: ActionRoute (Id Slot, Id Record)
deleteRecordSlot = action DELETE (pathJSON >/> pathSlotId </> pathId) $ \(si, ri) -> withAuth $ do
  guardVerfHeader
  slot <- getSlot PermissionEDIT si
  rec <- getRecord PermissionEDIT ri
  r <- moveRecordSlot (RecordSlot rec slot) emptySegment
  if r
  then return $ okResponse [] $ JSON.recordEncoding $ recordJSON False rec -- json not restricted because edit
  else return $ emptyResponse noContent204 []

deleteRecordAllSlot :: ActionRoute (Id Record)
deleteRecordAllSlot = action DELETE (pathJSON >/> "slot" >/> "all" >/> pathId) $ \ri -> withAuth $ do
  guardVerfHeader
  rec <- getRecord PermissionEDIT ri
  _ <- removeRecordAllSlot rec
  return $ okResponse [] $ JSON.recordEncoding $ recordJSON False rec -- json not restricted because edit
