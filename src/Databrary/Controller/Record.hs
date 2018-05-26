{-# LANGUAGE OverloadedStrings #-}
module Databrary.Controller.Record
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
import Data.Maybe (isNothing, fromMaybe)
import qualified Data.Text as T
import Network.HTTP.Types (noContent204, conflict409)

-- import Databrary.Ops
-- import Databrary.Has
import qualified Databrary.JSON as JSON
import Databrary.Action.Route
import Databrary.Action.Response
import Databrary.Action
import Databrary.Model.Id
import Databrary.Model.Volume
import Databrary.Model.Permission
import Databrary.Model.Record
import Databrary.Model.Category
import Databrary.Model.RecordSlot
import Databrary.Model.Metric
import Databrary.Model.Measure
import Databrary.Model.Segment
import Databrary.Model.Slot
import Databrary.HTTP.Form.Deform
import Databrary.HTTP.Path.Parser
import Databrary.Controller.Paths
import Databrary.Controller.Form
import Databrary.Controller.Volume
import Databrary.Controller.Slot
import Databrary.Controller.Permission
-- import Databrary.View.Record
import Databrary.View.Form (FormHtml)

getRecord :: Permission -> Id Record -> Handler Record
getRecord p i =
  checkPermission p =<< maybeAction =<< lookupRecord i

viewRecord :: ActionRoute (Id Record)
viewRecord = action GET (pathJSON >/> pathId) $ \i -> withAuth $ do
  rec <- getRecord PermissionPUBLIC i
  let v = recordVolume rec
  _ <- maybeAction (if volumeIsPublicRestricted v then Nothing else Just ()) -- block if restricted
  return $ okResponse [] $ JSON.recordEncoding $ recordJSON False rec -- json should consult volume
  -- HTML -> okResponse [] $ T.pack $ show $ recordId $ recordRow rec -- TODO

createRecord :: ActionRoute (Id Volume)
createRecord = action POST (pathJSON >/> pathId </< "record") $ \vi -> withAuth $ do
  vol <- getVolume PermissionEDIT vi
  br <- runForm (Nothing :: Maybe (RequestContext -> FormHtml a)) $ do
    csrfForm
    cat <- "category" .:> (deformMaybe' "No such category" . getCategory =<< deform)
    return $ blankRecord cat vol
  rec <- addRecord br
  return $ okResponse [] $ JSON.recordEncoding $ recordJSON False rec -- recordJSON not restricted because EDIT
  -- HTML -> peeks $ otherRouteResponse [] viewRecord (api, recordId $ recordRow rec)

postRecordMeasure :: ActionRoute (Id Record, Id Metric)
postRecordMeasure = action POST (pathJSON >/> pathId </> pathId) $ \(ri, mi) -> withAuth $ do
  record <- getRecord PermissionEDIT ri
  met <- maybeAction $ getMetric mi
  let mkMeasure datum = Measure record met datum
  rec' <- runForm (Nothing :: Maybe (RequestContext -> FormHtml a)) $ do
    csrfForm
    mDatum <- deformSync' ("datum" .:> deformNonEmpty deform)
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
  -- HTML -> peeks $ otherRouteResponse [] viewRecord (api, recordId $ recordRow rec')

deleteRecord :: ActionRoute (Id Record)
deleteRecord = action DELETE (pathJSON >/> pathId) $ \ri -> withAuth $ do
  guardVerfHeader
  rec <- getRecord PermissionEDIT ri
  r <- removeRecord rec
  unless r $ result $
    response conflict409 [] $ JSON.recordEncoding $ recordJSON False rec -- json not restricted because edit
    -- HTML -> response conflict409 [] ("This record is still used" :: T.Text)
  return $ emptyResponse noContent204 []
  -- HTML -> peeks $ otherRouteResponse [] viewVolume (api, view rec)

postRecordSlot :: ActionRoute (Id Slot, Id Record)
postRecordSlot = action POST (pathJSON >/> pathSlotId </> pathId) $ \(si, ri) -> withAuth $ do
  slot <- getSlot PermissionEDIT Nothing si
  rec <- getRecord PermissionEDIT ri
  src <- runForm Nothing $ do
    csrfForm
    "src" .:> deformNonEmpty deform
  r <- moveRecordSlot (RecordSlot rec slot{ slotSegment = fromMaybe emptySegment src }) (slotSegment slot)
  -- HTML | r      -> peeks $ otherRouteResponse [] (viewSlot False) (api, (Just (view slot), slotId slot))
  --    | otherwise -> peeks $ otherRouteResponse [] viewRecord (api, recordId $ recordRow rec)
  if r
  then return $ okResponse [] $ JSON.recordEncoding $ recordSlotJSON False (RecordSlot rec slot)
  else return $ okResponse [] $ JSON.recordEncoding $ recordJSON False rec -- recordJSON not restricted because EDIT

deleteRecordSlot :: ActionRoute (Id Slot, Id Record)
deleteRecordSlot = action DELETE (pathJSON >/> pathSlotId </> pathId) $ \(si, ri) -> withAuth $ do
  guardVerfHeader
  slot <- getSlot PermissionEDIT Nothing si
  rec <- getRecord PermissionEDIT ri
  r <- moveRecordSlot (RecordSlot rec slot) emptySegment
    -- HTML | r -> peeks $ otherRouteResponse [] viewRecord (api, recordId $ recordRow rec)
  if r
  then return $ okResponse [] $ JSON.recordEncoding $ recordJSON False rec -- json not restricted because edit
  else return $ emptyResponse noContent204 []

deleteRecordAllSlot :: ActionRoute (Id Record)
deleteRecordAllSlot = action DELETE (pathJSON >/> "slot" >/> "all" >/> pathId) $ \ri -> withAuth $ do
  guardVerfHeader
  rec <- getRecord PermissionEDIT ri
  _ <- removeRecordAllSlot rec
  -- HTML -> peeks $ otherRouteResponse [] viewRecord (api, recordId $ recordRow rec)
  return $ okResponse [] $ JSON.recordEncoding $ recordJSON False rec -- json not restricted because edit
