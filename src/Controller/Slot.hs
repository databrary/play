{-# LANGUAGE OverloadedStrings #-}
module Controller.Slot
  ( getSlot
  , viewSlot
  , slotDownloadName
  , thumbSlot
  ) where

import Control.Monad (when, mfilter)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import Data.Maybe (isJust)
import Data.Monoid ((<>))
import qualified Data.Text as T
import Network.HTTP.Types.Status (movedPermanently301)
import qualified Network.Wai as Wai

import Has (view, peeks)
import qualified JSON as JSON
import Model.Id
import Model.Permission hiding (checkPermission)
import Model.Volume
import Model.Container
import Model.Slot
import Model.Asset
import Model.AssetSlot
import Model.AssetSegment
import Model.Excerpt
import Model.Record
import Model.RecordSlot
import Model.Tag
import Model.Comment
import Store.Filename
import HTTP.Path.Parser
import Action
import Controller.Paths
import Controller.Permission
import Controller.Angular
import Controller.Container
import Controller.Volume (volumeIsPublicRestricted)
import Controller.Web
import {-# SOURCE #-} Controller.AssetSegment

getSlot :: Permission -> Maybe (Id Volume) -> Id Slot -> Handler Slot
getSlot p mv i =
  checkPermissionOld p =<< maybeAction . maybe id (\v -> mfilter $ (v ==) . volumeId . volumeRow . containerVolume . slotContainer) mv =<< lookupSlot i

slotJSONField :: Bool -> Slot -> BS.ByteString -> Maybe BS.ByteString -> Handler (Maybe JSON.Encoding)
slotJSONField getOrig o "assets" _ =
  case getOrig of
       True -> Just . JSON.mapRecords (assetSlotJSON False) <$> lookupOrigSlotAssets o -- public restricted consult volume soon
       False -> Just . JSON.mapRecords (assetSlotJSON False) <$> lookupSlotAssets o
slotJSONField _ o "records" _ =  -- recordJSON should decide public restricted based on volume
  Just . JSON.mapRecords
    (\r ->
       recordSlotJSON False r `JSON.foldObjectIntoRec` ("record" JSON..=: recordJSON False (slotRecord r))) <$> lookupSlotRecords o
slotJSONField _ o "tags" n = do
  tc <- lookupSlotTagCoverage o (maybe 64 fst $ BSC.readInt =<< n)
  return $ Just $ JSON.pairs $ JSON.recordMap $ map tagCoverageJSON tc
slotJSONField _ o "comments" n = do
  c <- lookupSlotComments o (maybe 64 fst $ BSC.readInt =<< n)
  return $ Just $ JSON.mapRecords commentJSON c
slotJSONField _ o "excerpts" _ =
  Just . JSON.mapObjects (\e -> excerptJSON e <> "asset" JSON..= (view e :: Id Asset)) <$> lookupSlotExcerpts o
slotJSONField _ o "filename" _ =
  return $ Just $ JSON.toEncoding $ makeFilename $ slotDownloadName o
slotJSONField _ _ _ _ = return Nothing

slotJSONQuery :: Bool -> Slot -> JSON.Query -> Handler (JSON.Record (Id Container) JSON.Series)
slotJSONQuery origQ o q = (slotJSON o `JSON.foldObjectIntoRec`) <$> JSON.jsonQuery (slotJSONField origQ o) q

slotDownloadName :: Slot -> [T.Text]
slotDownloadName s = containerDownloadName (slotContainer s)

viewSlot :: Bool -> ActionRoute (API, (Maybe (Id Volume), Id Slot))
viewSlot viewOrig = action GET (pathAPI </> pathMaybe pathId </> pathSlotId) $ \(api, (vi, i)) -> withAuth $ do
  when (api == HTML && isJust vi) angular
  c <- getSlot PermissionPUBLIC vi i
  let v = (containerVolume . slotContainer) c
  _ <- maybeAction (if volumeIsPublicRestricted v then Nothing else Just ()) -- block if restricted
  case api of
    JSON -> okResponse [] <$> (slotJSONQuery viewOrig c =<< peeks Wai.queryString)
    HTML
      | isJust vi -> return $ okResponse [] $ BSC.pack $ show $ containerId $ containerRow $ slotContainer c
      | otherwise ->
          peeks $ redirectRouteResponse movedPermanently301 [] (viewSlot viewOrig) (api, (Just ((volumeId . volumeRow . containerVolume . slotContainer) c), slotId c))

thumbSlot :: ActionRoute (Maybe (Id Volume), Id Slot)
thumbSlot = action GET (pathMaybe pathId </> pathSlotId </< "thumb") $ \(vi, i) -> withAuth $ do
  s <- getSlot PermissionPUBLIC vi i
  let v = (containerVolume . slotContainer) s
  _ <- maybeAction (if volumeIsPublicRestricted v then Nothing else Just ()) -- block if restricted, duplicated from above
  e <- lookupSlotSegmentThumb s
  maybe
    (peeks $ otherRouteResponse [] webFile (Just $ staticPath ["images", "draft.png"]))
    (\as -> peeks $ otherRouteResponse [] downloadAssetSegment (slotId $ view as, assetId $ assetRow $ view as))
    e
