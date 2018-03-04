{-# LANGUAGE OverloadedStrings #-}
module Databrary.Routes
  ( routeMap
  , newRouteMap
  ) where

import qualified Data.ByteString as BS
import Web.Route.Invertible (RouteMap, routes, routeCase)
import qualified Network.Wai.Route as WAR
import qualified Network.Wai as WAI
import qualified Network.HTTP.Types.Method as HTM

import Databrary.Action
import Databrary.Controller.Root
import Databrary.Controller.Login
import Databrary.Controller.Register
import Databrary.Controller.Token
import Databrary.Controller.Party
import Databrary.Controller.Authorize
import Databrary.Controller.Volume
import Databrary.Controller.VolumeAccess
import Databrary.Controller.Funding
import Databrary.Controller.Container
import Databrary.Controller.Slot
import Databrary.Controller.Record
import Databrary.Controller.Metric
import Databrary.Controller.Citation
import Databrary.Controller.Upload
import Databrary.Controller.Format
import Databrary.Controller.Asset
import Databrary.Controller.AssetSegment
import Databrary.Controller.Excerpt
import Databrary.Controller.Zip
import Databrary.Controller.Tag
import Databrary.Controller.Comment
import Databrary.Controller.CSV
import Databrary.Controller.VolumeState
import Databrary.Controller.Activity
import Databrary.Controller.Transcode
import Databrary.Controller.Ingest
import Databrary.Controller.Web
import Databrary.Controller.Search
import Databrary.Controller.Periodic
import Databrary.Controller.Notification
import Databrary.Action.Run (runAction)
import Databrary.Service.Types (Service)

newRouteMap :: Service -> [(BS.ByteString, WAR.Handler IO)]
newRouteMap routeContext =
    [   ("", hn (viewRootHandler HTML)) -- (\ps req resp -> runAction routeContext (viewRootHandler HTML ps) req resp))
      , ("/", hn (viewRootHandler HTML))
      , ("/api", hn (viewRootHandler JSON))
      , ("/robots.txt", hn viewRobotsTxtHandler)

      , ("/api/user", hn (userHandler JSON))
      , ("/user", hn (userHandler HTML))
      , ("/api/user/login", hnm (loginHandler JSON))
      , ("/user/login", hnm (loginHandler HTML))
      , ("/user/logout", hn (postLogoutHandler HTML))
      , ("/api/user/logout", hn (postLogoutHandler JSON))
      , ("/user/register", hnm (registerHandler HTML))
      , ("/api/user/register", hnm (registerHandler JSON))
      , ("/user/password", hnm (passwordResetHandler HTML))
      , ("/api/user/password", hnm (passwordResetHandler JSON))
      -- login token x 2
      , ("/party/:partyId/investigator", hn resendInvestigatorHandler)

      -- , ("/party/:partyId", hnm (partyHandler HTML))  -- get, post
      -- , ("/profile", hnm (partyHandler JSON)) -- get, post
      -- , ("/api/party/:partyId", hnm (partyHandler JSON)) -- get, post
      -- , ("/api/profile", hnm (partyHandler JSON)) -- get, post
      -- , ("/party/party/:partyId/edit", hn viewPartyEditHandler)  -- get
      -- , ("/party/profile/edit", hn viewPartyEditHandler)  -- get
      -- , ("/party/create", hn viewPartyCreateHandler)  -- get  <<<<<<<<<<
      -- , route viewAuthorize
      -- , route postAuthorize
      -- , route deleteAuthorize
      -- , route postAuthorizeNotFound
      -- , ("/party/:partyId/avatar", hn avatarHandler)  -- get 
      -- , route viewPartyActivity  -- 2nd pass
      --, ("/party", hnm (createPartyHandler HTML)) -- post, get   <<<<<<<<<
      --, ("/api/party", hnm (createPartyHandler JSON)) -- post, get    <<<<<<<<
      --, ("/party/:partyId/delete", hnm (deletePartyHandler HTML)) -- post, get
      --, ("/party/admin", hn adminPartiesHandler) -- get        <<<<<<<
      --, ("/party/csv", hn csvPartiesHandler) -- get            <<<<<<<
      --, ("/party/duplicate/csv", hn csvDuplicatePartiesHandler) -- get    <<<<<<<
      
        -- , route viewVolume
        -- , route postVolume
        -- , route viewVolumeEdit
        -- , route viewVolumeAccess
        -- , route postVolumeAccess
        -- , route viewVolumeLinks
        -- , route postVolumeLinks
        -- , route postVolumeFunding  -- 2nd pass
        -- , route deleteVolumeFunder  -- 2nd pass
        -- , route postVolumeAssist
        -- , route viewVolumeCreate <<<<
        -- , route createVolume  <<<<<<
        -- , route queryVolumes  <<<<<
        -- , route $ zipVolume False 
        -- , route $ zipVolume True 
        -- , route viewVolumeDescription
        -- , route thumbVolume
        -- , route csvVolume
        -- , route viewVolumeActivity -- 2nd pass

        -- , route createContainer  -- all 2nd pass
        -- , route $ viewSlot False 
        -- , route viewContainerEdit
        -- , route postContainer
        -- , route deleteContainer
        -- , route viewContainerActivity  -- 2nd pass
        -- , route $ zipContainer False 
        -- , route $ zipContainer True 
        -- , route thumbSlot

        -- , route viewFormats  <<<<<<<<<

        -- , route viewAsset  -- all 2nd pass
        -- , route postAsset
        -- , route viewAssetEdit
        -- , route deleteAsset
        -- , route downloadAsset
        -- , route downloadOrigAsset 
        -- , route thumbAsset
        -- , route viewAssetCreate
        -- , route createAsset
        -- , route createSlotAsset
        -- , route viewSlotAssetCreate

        -- , route (viewAssetSegment False)  -- all 2nd pass
        -- , route downloadAssetSegment 
        -- , route downloadOrigAssetSegment 
        -- , route (thumbAssetSegment False) 
        -- , route postExcerpt
        -- , route deleteExcerpt

        -- , route createRecord  -- all 2nd pass
        -- , route viewRecord
        -- , route postRecordMeasure
        -- , route deleteRecord
        -- , route postRecordSlot
        -- , route deleteRecordSlot
        -- , route deleteRecordAllSlot

        -- , route postVolumeMetric  -- all 2nd pass
        -- , route deleteVolumeMetric
        -- , route postVolumeState
        -- , route deleteVolumeState

        -- , route queryTags -- 2nd pass
        -- , route postTag -- 2nd pass
        -- , route deleteTag -- 2nd pass
        -- , route postComment -- 2nd pass

        -- , route postSearch <<<<<<<<<<<

        -- , route uploadStart  -- 2nd pass
        -- , route uploadChunk  <<<<<<<<<<<
        -- , route testChunk   <<<<<<<<<


      , ("/api/constants", hn viewConstantsHandler)
        -- , route getCitation  <<<<
        -- , route queryFunder <<<<
        -- , route remoteTranscode  -- second pass
        -- , route viewSiteActivity <<<<<<<<

        -- , route viewNotifications <<<<<<<<
        -- , route deleteNotification -- second pass
        -- , route deleteNotifications -- second pass
        -- , route viewNotify  <<<<<<<<<<
        -- , route postNotify <<<<<<<<<

      , ("/admin/transcode", hn0 viewTranscodesHandler)
        -- , route postTranscode  -- second pass
        -- , route viewIngest -- second pass
        -- , route postIngest  -- second pass
      , ("/admin/periodic", hnm periodicHandler)

        -- , route webFile -- hard?

        -- hack to override not found
      -- TODO: add below back? can clash with above
      {-
      , ("/:a", (\ps req respond -> runAction routeContext (notFoundResponseHandler ps) req respond))
      -}
    ]
  where
    hn0 :: Action -> WAR.Handler IO  -- make handler
    hn0 action = \ps req responder -> runAction routeContext action req responder
    hn :: ([(BS.ByteString, BS.ByteString)] -> Action) -> WAR.Handler IO  -- make handler
    hn mkAction = \ps req responder -> runAction routeContext (mkAction ps) req responder
    hnm :: (HTM.Method -> [(BS.ByteString, BS.ByteString)] -> Action) -> WAR.Handler IO  -- make handler with method
    hnm mkAction = \ps req responder -> runAction routeContext (mkAction (WAI.requestMethod req) ps) req responder

routeMap :: RouteMap Action
routeMap = routes
  [
  --   route viewRoot
  -- , route viewRobotsTxt

  --  route viewUser
  --, route postUser
  --  route viewLogin
  -- , route postLogin
  --  route postLogout
  --  route viewRegister
  --, route postRegister
  --  route viewPasswordReset
  -- , route postPasswordReset
    route viewLoginToken
  , route postPasswordToken
  -- , route resendInvestigator

  , route viewParty
  , route postParty
  , route viewPartyEdit
  , route viewPartyCreate
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
  , route adminParties
  , route csvParties
  , route csvDuplicateParties

  , route viewVolume
  , route postVolume
  , route viewVolumeEdit
  , route viewVolumeAccess
  , route postVolumeAccess
  , route viewVolumeLinks
  , route postVolumeLinks
  , route postVolumeFunding
  , route deleteVolumeFunder
  , route postVolumeAssist
  , route viewVolumeCreate
  , route createVolume
  , route queryVolumes
  , route $ zipVolume False 
  , route $ zipVolume True 
  , route viewVolumeDescription
  , route thumbVolume
  , route csvVolume
  , route viewVolumeActivity

  , route createContainer
  , route $ viewSlot False 
  , route viewContainerEdit
  , route postContainer
  , route deleteContainer
  , route viewContainerActivity
  , route $ zipContainer False 
  , route $ zipContainer True 
  , route thumbSlot

  , route viewFormats

  , route viewAsset
  , route postAsset
  , route viewAssetEdit
  , route deleteAsset
  , route downloadAsset
  , route downloadOrigAsset 
  , route thumbAsset
  , route viewAssetCreate
  , route createAsset
  , route createSlotAsset
  , route viewSlotAssetCreate

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
  , route postComment

  , route postSearch

  , route uploadStart
  , route uploadChunk
  , route testChunk

  -- , route viewConstants
  , route getCitation
  , route queryFunder
  , route remoteTranscode
  , route viewSiteActivity

  , route viewNotifications
  , route deleteNotification
  , route deleteNotifications
  , route viewNotify
  , route postNotify

  -- , route viewTranscodes
  , route postTranscode
  , route viewIngest
  , route postIngest
  -- , route viewPeriodic
  -- , route postPeriodic

  , route webFile
  ] where
  route = routeCase
