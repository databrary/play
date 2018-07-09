{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
-- | This module describes the routes served by Databrary.
--
-- It is a tale of systems that evolve over time. Writing these words on
-- 2018-05-23, I am beginning to serve routes with Servant. Meanwhile, ~90
-- routes are still served by the original system, web-inv-routes; and ~30 are
-- served by Wai.Route as a temporary stopgap.
--
-- This module glues the API description to a particular service implementation.
-- See "API" for a pure description of the Servant-described API.
module Routes
  (
  -- * Temporary measures: Wai.Route
    routeMapWai
  -- * OG route descriptions: web-inv-routes
  , routeMapInvertible
  ) where

import Web.Route.Invertible (RouteMap, routes, routeCase)
import qualified Data.ByteString as BS
import qualified Network.HTTP.Types.Method as HTM
import qualified Network.Wai as WAI
import qualified Network.Wai.Route as WaiRoute

import Action
import Controller.Root
import Controller.Login
import Controller.Register
import Controller.Token
import Controller.Party
import Controller.Authorize
import Controller.Volume
import Controller.VolumeAccess
import Controller.Funding
import Controller.Container
import Controller.Slot
import Controller.Record
import Controller.Metric
import Controller.Citation
import Controller.Upload
import Controller.Format
import Controller.Asset
import Controller.AssetSegment
import Controller.Excerpt
import Controller.Zip
import Controller.Tag
-- import Controller.Comment
import Controller.CSV
import Controller.VolumeState
import Controller.Activity
import Controller.Transcode
import Controller.Ingest
import Controller.Web
import Controller.Search
import Controller.Periodic
import Controller.Notification
import Action.Run (actionApp)
import Service.Types (Service)

-- | Map of route actions managed by Wai Routes.
routeMapWai :: Service -> [(BS.ByteString, WaiRoute.Handler IO)]
routeMapWai routeContext =
    [   ("", hn (viewRootHandler HTML)) -- no params/use hn0
      , ("/", hn (viewRootHandler HTML)) -- no params/use hn0
      , ("/api", hn (viewRootHandler JSON)) -- no params/use hn0
      , ("/robots.txt", hn viewRobotsTxtHandler) -- no params/use hn0
      , ("/api/user", hn (userHandler JSON)) -- no params/use hn0
      , ("/user", hn (userHandler HTML)) -- no params/use hn0
      , ("/api/user/login", hnm (loginHandler JSON)) -- no params/use hn0
      , ("/user/login", hnm (loginHandler HTML)) -- no params/use hn0
      , ("/user/logout", hn (postLogoutHandler HTML)) -- no params/use hn0
      , ("/api/user/logout", hn (postLogoutHandler JSON)) -- no params/use hn0
      , ("/user/register", hnm (registerHandler HTML)) -- no params/use hn0
      , ("/api/user/register", hnm (registerHandler JSON)) -- no params/use hn0
      , ("/user/password", hnm (passwordResetHandler HTML)) -- no params/use hn0
      , ("/api/user/password", hnm (passwordResetHandler JSON)) -- no params/use hn0
      , ("/party/:partyId/investigator", hn resendInvestigatorHandler)
      , ("/party/create", hn0 viewPartyCreateHandler)
      , ("/party/admin", hn0 adminPartiesHandler)
      , ("/party/csv", hn0 csvPartiesHandler)
      , ("/party/duplicate/csv", hn0 csvDuplicatePartiesHandler)
      , ("/volume/create", hn0 viewVolumeCreateHandler)
      , ("asset/formats", hn0 viewFormatsHandler)
      , ("/search", hn0 (postSearchHandler HTML))
      , ("/api/search", hn0 (postSearchHandler JSON))
      , ("/api/constants", hn viewConstantsHandler) -- no params/use hn0
      , ("/api/cite", hn0 getCitationHandler)
      , ("/api/funder", hn0 queryFunderHandler)
      , ("/api/activity", hn0 viewSiteActivityHandler)
      , ("/admin/transcode", hn0 viewTranscodesHandler)
      , ("/admin/periodic", hnm periodicHandler) -- no params/use hn0
    ]
  where
    hn0 :: Action -> WaiRoute.Handler IO  -- make handler
    hn0 act = \_ req responder -> actionApp routeContext act req responder
    hn :: ([(BS.ByteString, BS.ByteString)] -> Action) -> WaiRoute.Handler IO  -- make handler
    hn mkAction = \ps req responder -> actionApp routeContext (mkAction ps) req responder
    hnm :: (HTM.Method -> [(BS.ByteString, BS.ByteString)] -> Action) -> WaiRoute.Handler IO  -- make handler with method
    hnm mkAction = \ps req responder -> actionApp routeContext (mkAction (WAI.requestMethod req) ps) req responder

-- | Map of route actions handled by web-inv-routes.
routeMapInvertible :: RouteMap Action
routeMapInvertible = routes
  [ route viewLoginToken
  , route postPasswordToken
  , route viewParty
  , route postParty
  , route viewPartyEdit
  , route viewPartyDelete
  , route viewAuthorize
  , route postAuthorize
  , route deleteAuthorize
  , route postAuthorizeNotFound
  , route viewAvatar
  , route viewPartyActivity
  , route createParty
  , route deleteParty
  , route queryParties
  , route viewVolume
  , route postVolume
  , route viewVolumeEdit
  , route postVolumeAccess
  , route postVolumeLinks
  , route postVolumeFunding
  , route deleteVolumeFunder
  , route postVolumeAssist
  , route createVolume
  , route queryVolumes
  , route (zipVolume False)
  , route (zipVolume True)
  , route viewVolumeDescription
  , route thumbVolume
  , route csvVolume
  , route viewVolumeActivity
  , route createContainer
  , route (viewSlot False)
  , route viewContainerEdit
  , route postContainer
  , route deleteContainer
  , route viewContainerActivity
  , route (zipContainer False)
  , route (zipContainer True)
  , route thumbSlot
  , route viewAsset
  , route postAsset
  , route deleteAsset
  , route downloadAsset
  , route downloadOrigAsset
  , route thumbAsset
  , route createAsset
  , route createSlotAsset
  , route (viewAssetSegment False)
  , route downloadAssetSegment
  , route downloadOrigAssetSegment
  , route (thumbAssetSegment False)
  , route postExcerpt
  , route deleteExcerpt
  , route createRecord
  , route viewRecord
  , route postRecordMeasure
  , route deleteRecord
  , route postRecordSlot
  , route deleteRecordSlot
  , route deleteRecordAllSlot
  , route postVolumeMetric
  , route deleteVolumeMetric
  , route postVolumeState
  , route deleteVolumeState
  , route queryTags
  , route postTag
  , route deleteTag
  -- , route postComment
  , route uploadStart
  , route uploadChunk
  , route testChunk
  , route remoteTranscode
  , route viewNotifications
  , route deleteNotification
  , route deleteNotifications
  , route viewNotify
  , route postNotify
  , route postTranscode
  , route viewIngest
  , route postIngest
  , route detectParticipantCSV
  , route runParticipantUpload
  , route webFile
  ] where
  route = routeCase
