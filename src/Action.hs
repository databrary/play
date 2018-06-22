{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
module Action
  ( Request
  , RequestContext
  , Databrary.Handler
  , Action

  , Response
  , response
  , emptyResponse
  , redirectRouteResponse
  , otherRouteResponse
  , forbiddenResponse
  , notFoundResponse
  , okResponse
  , result
  , maybeAction

  , module Action.Route

  , withAuth
  , withoutAuth
  -- * Building the application
  , WaiRouteApp(..)
  , ActionRouteApp (..)
  , actionRouteApp
  ) where

import Network.HTTP.Types
    (Status, seeOther303, forbidden403, notFound404, ResponseHeaders, hLocation)
import Servant
import qualified Data.ByteString.Builder as BSB
import qualified Data.ByteString.Lazy as BSL
import qualified Network.Wai as Wai
import qualified Web.Route.Invertible.Wai as Invertible

import Has (peeks)
import HTTP.Request
import Action.Types as Databrary
import Action.Run
import Action.Response
import Action.Route
import Service.Types
import View.Error

-- | Redirect a request to a new route
redirectRouteResponse
    :: Status
    -> ResponseHeaders
    -> Invertible.RouteAction r a
    -> r
    -> Request
    -> Response
redirectRouteResponse status hdrs ra r req =
    emptyResponse status (locationHeader : hdrs)
  where
    locationHeader =
        (hLocation, build (actionURL (Just req) ra r (Wai.queryString req)))
    build = BSL.toStrict . BSB.toLazyByteString

-- | Redirect with HTTP code 303
otherRouteResponse
    :: ResponseHeaders -> Invertible.RouteAction r a -> r -> Request -> Response
otherRouteResponse = redirectRouteResponse seeOther303

-- | HTTP code 403
forbiddenResponse :: RequestContext -> Response
forbiddenResponse = response forbidden403 [] . htmlForbidden

-- | HTTP code 404
notFoundResponse :: RequestContext -> Response
notFoundResponse = response notFound404 [] . htmlNotFound

-- | Fail with 404 if not 'Just'
maybeAction :: Maybe a -> Databrary.Handler a
maybeAction (Just a) = return a
maybeAction Nothing = result =<< peeks notFoundResponse

newtype WaiRouteApp = WaiRouteApp Application
newtype ActionRouteApp = ActionRouteApp Application

-- | The second level of the Databrary 'web framework'. Makes a (wrapped) Wai
-- Application given a route map, a hatch into the Wai Route fallback, and the
-- already-generated system capabilities.
--
-- Most routes are served by web-inv-route—style ActionRoutes, but some (~30%)
-- have been converted to Wai Routes.
actionRouteApp
    :: Invertible.RouteMap Action
    -- ^ The original route map. Now partially replaced by Wai Routes
    -> WaiRouteApp
    -- ^ The newer Wai Route-based Application
    -> Service
    -- ^ System capabilities
    -> ActionRouteApp
    -- ^ The actual web app
actionRouteApp invMap (WaiRouteApp waiRouteApp) svc = ActionRouteApp
    (\waiRequest waiSend ->
        -- Use the original Action if it still exists
        either
            (waiRouteFallback waiRequest waiSend)
            (\act -> actionApp svc act waiRequest waiSend)
            (Invertible.routeWai waiRequest invMap)
    )
  where
    waiRouteFallback waiRequest waiSend (st,hdrs)
        -- Currently, this might be only possible error result?
        | st == notFound404 = waiRouteApp waiRequest waiSend
        -- Handle any other possible errors with the original app's error
        -- handling.
        | otherwise = actionApp svc (err (st,hdrs)) waiRequest waiSend
    -- This is almost, but not quite, equal to 'notFoundResponseHandler'
    err :: (Status, ResponseHeaders) -> Action
    err (status, headers) =
        withoutAuth (peeks (response status headers . htmlNotFound))
