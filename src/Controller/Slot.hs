{-# LANGUAGE OverloadedStrings #-}
module Controller.Slot
    ( getSlot
    , getVolumeSlot
    , viewSlot
    , slotDownloadName
    , thumbSlot
    ) where

import Control.Monad (when)
import Data.Maybe (isJust)
import Data.Monoid ((<>))
import Network.HTTP.Types.Status (movedPermanently301)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.Text as T
import qualified Network.Wai as Wai

import Action
import Controller.Angular
import {-# SOURCE #-} Controller.AssetSegment
import Controller.Container
import Controller.Paths
import Controller.Volume (volumeIsPublicRestricted)
import Controller.Web
import HTTP.Path.Parser
import Has (view, peeks)
import Model.Asset
import Model.AssetSegment
import Model.AssetSlot
import Model.Comment
import Model.Container
import Model.Excerpt
import Model.Id
import Model.Permission hiding (checkPermission)
import Model.Record
import Model.RecordSlot
import Model.Slot
import Model.Tag
import Model.Volume
import Store.Filename
import qualified JSON

-- | Convert a 'Slot' into HTTP error responses if the lookup fails or is
-- denied.
--
-- NOTE: Intentionally implemented exactly like getVolume. Implementations
-- should be collected in a single module and merged.
getSlot
    :: Permission
    -- ^ Requested permission
    -> Id Slot
    -- ^ Slot to look up
    -> Handler Slot
    -- ^ The slot, as requested (or a short-circuited error response)
getSlot requestedPerm sId = do
    res <- requestSlot requestedPerm sId
    case res of
        LookupFailed -> result =<< peeks notFoundResponse
        RequestDenied -> result =<< peeks forbiddenResponse
        RequestResult s -> pure s

-- | Look up a Slot and confirm that it is associated with the given Volume.
--
-- This method exists, presumably, so that we can construct urls like
-- volume/:volId/slot/:slotId and make sure there's no funny business going on.
getVolumeSlot
    :: Id Volume
    -- ^ Associated Volume
    -> Permission
    -- ^ Requested permission
    -> Id Slot
    -- ^ Slot to look up
    -> Handler Slot
    -- ^ The slot, as requested (or a short-circuited error response)
getVolumeSlot volId requestedPerm sId = do
    s <- getSlot requestedPerm sId
    if volumeId (volumeRow (containerVolume (slotContainer s))) == volId
        then pure s
        else result =<< peeks notFoundResponse

slotJSONField
    :: Bool
    -> Slot
    -> BS.ByteString
    -> Maybe BS.ByteString
    -> Handler (Maybe JSON.Encoding)
slotJSONField getOrig o "assets" _ = case getOrig of
    True ->
        Just . JSON.mapRecords (assetSlotJSON False) <$> lookupOrigSlotAssets o -- public restricted consult volume soon
    False ->
        Just . JSON.mapRecords (assetSlotJSON False) <$> lookupSlotAssets o
slotJSONField _ o "records" _ =  -- recordJSON should decide public restricted based on volume
    Just
        . JSON.mapRecords
            (\r ->
                recordSlotJSON False r
                    `JSON.foldObjectIntoRec` ("record" JSON..=: recordJSON
                                                False
                                                (slotRecord r)
                                             )
            )
        <$> lookupSlotRecords o
slotJSONField _ o "tags" n = do
    tc <- lookupSlotTagCoverage o (maybe 64 fst $ BSC.readInt =<< n)
    return $ Just $ JSON.pairs $ JSON.recordMap $ map tagCoverageJSON tc
slotJSONField _ o "comments" n = do
    c <- lookupSlotComments o (maybe 64 fst $ BSC.readInt =<< n)
    return $ Just $ JSON.mapRecords commentJSON c
slotJSONField _ o "excerpts" _ =
    Just
        . JSON.mapObjects
            (\e -> excerptJSON e <> "asset" JSON..= (view e :: Id Asset))
        <$> lookupSlotExcerpts o
slotJSONField _ o "filename" _ =
    return $ Just $ JSON.toEncoding $ makeFilename $ slotDownloadName o
slotJSONField _ _ _ _ = return Nothing

slotJSONQuery
    :: Bool
    -> Slot
    -> JSON.Query
    -> Handler (JSON.Record (Id Container) JSON.Series)
slotJSONQuery origQ o q =
    (slotJSON o `JSON.foldObjectIntoRec`)
        <$> JSON.jsonQuery (slotJSONField origQ o) q

slotDownloadName :: Slot -> [T.Text]
slotDownloadName s = containerDownloadName (slotContainer s)

viewSlot :: Bool -> ActionRoute (API, (Maybe (Id Volume), Id Slot))
viewSlot viewOrig =
    action GET (pathAPI </> pathMaybe pathId </> pathSlotId)
        $ \(api, (vi, i)) -> withAuth $ do
            when (api == HTML && isJust vi) angular
            c <- (maybe getSlot getVolumeSlot vi) PermissionPUBLIC i
            let v = (containerVolume . slotContainer) c
            _ <- maybeAction
                (if volumeIsPublicRestricted v then Nothing else Just ()) -- block if restricted
            case api of
                JSON ->
                    okResponse []
                        <$> (slotJSONQuery viewOrig c =<< peeks Wai.queryString)
                HTML
                    | isJust vi
                    -> return
                        $ okResponse []
                        $ BSC.pack
                        $ show
                        $ containerId
                        $ containerRow
                        $ slotContainer c
                    | otherwise
                    -> peeks $ redirectRouteResponse
                        movedPermanently301
                        []
                        (viewSlot viewOrig)
                        ( api
                        , ( Just
                            ((volumeId
                             . volumeRow
                             . containerVolume
                             . slotContainer
                             )
                                c
                            )
                          , slotId c
                          )
                        )

thumbSlot :: ActionRoute (Maybe (Id Volume), Id Slot)
thumbSlot =
    action GET (pathMaybe pathId </> pathSlotId </< "thumb") $ \(vi, i) ->
        withAuth $ do
            s <- (maybe getSlot getVolumeSlot vi) PermissionPUBLIC i
            let v = (containerVolume . slotContainer) s
            _ <- maybeAction
                (if volumeIsPublicRestricted v then Nothing else Just ()) -- block if restricted, duplicated from above
            e <- lookupSlotSegmentThumb s
            maybe
                (peeks $ otherRouteResponse
                    []
                    webFile
                    (Just $ staticPath ["images", "draft.png"])
                )
                (\as -> peeks $ otherRouteResponse
                    []
                    downloadAssetSegment
                    (slotId $ view as, assetId $ assetRow $ view as)
                )
                e
