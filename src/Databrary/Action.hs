{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
module Databrary.Action
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

  , module Databrary.Action.Route

  , withAuth
  , withoutAuth
  -- * Building the application
  , WaiRouteApp(..)
  , actionRouteApp
  ) where

import Network.HTTP.Types
    (Status, seeOther303, forbidden403, notFound404, ResponseHeaders, hLocation)
import Servant
import qualified Data.ByteString.Builder as BSB
import qualified Data.ByteString.Lazy as BSL
import qualified Network.Wai as Wai
import qualified Web.Route.Invertible.Wai as Invertible

import Databrary.Has (peeks)
import Databrary.HTTP.Request
import Databrary.API
import Databrary.Action.Types as Databrary
import Databrary.Action.Run
import Databrary.Action.Response
import Databrary.Action.Route
import Databrary.Service.Types
import Databrary.View.Error

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

-- | Create a server to serve the ServantAPI.
apiServer :: WaiRouteApp -> Server ServantAPI
apiServer (WaiRouteApp app') =
    pure "this" :<|> Tagged app'

-- Build a Wai.Application out of our Servant server plus the Wai Route escape
-- hatch.
servantApp :: WaiRouteApp -> Application
servantApp waiRouteApp =
    serve servantAPI (apiServer waiRouteApp)

-- | The lowest level of the Databrary 'web framework'. Makes a Wai Application
-- given a route map, a hatch into the Wai Route fallback, and the
-- already-generated system capabilities.
actionRouteApp
    :: Invertible.RouteMap Action
    -- ^ The original route map. Now partially replaced by Servant and Wai
    -- Routes
    -> WaiRouteApp
    -- ^ The newer Wai Route-based Application
    -> Service
    -- ^ System capabilities
    -> Wai.Application
    -- ^ The actual web app
actionRouteApp invMap waiRouteApp routeContext req =
    -- Route lookup
    let eMatchedAction :: Either (Status, ResponseHeaders) Action
        eMatchedAction = Invertible.routeWai req invMap
    in
      case eMatchedAction of
        Right act ->
            -- Still handled by invertible routes
            actionApp routeContext act req
        Left (st,hdrs) ->
            if st == notFound404 -- currently, this might be only possible error result?
            then
                -- Our hatch to the WaiRouteApp is Servant!
                servantApp waiRouteApp req
            else
                actionApp routeContext (err (st,hdrs)) req
  where
    -- This is almost, but not quite, equal to 'notFoundResponseHandler'
    err :: (Status, ResponseHeaders) -> Action
    err (status, headers) =
        withoutAuth (peeks (response status headers . htmlNotFound))
