{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module Databrary.Action
  ( Request
  , RequestContext
  , ActionM
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
  , runActionRoute
  ) where

import qualified Data.ByteString.Builder as BSB
import qualified Data.ByteString.Lazy as BSL
import Network.HTTP.Types (Status, seeOther303, forbidden403, notFound404, ResponseHeaders, hLocation)
import qualified Network.Wai as Wai
import qualified Web.Route.Invertible.Wai as R
import qualified Network.Wai.Route as WAR

import Databrary.Has (peeks)
import Databrary.HTTP.Request
import Databrary.Action.Types
import Databrary.Action.Run
import Databrary.Action.Response
import Databrary.Action.Route
import Databrary.Service.Types
import Databrary.View.Error

redirectRouteResponse :: Status -> ResponseHeaders -> R.RouteAction r a -> r -> Request -> Response
redirectRouteResponse s h r a req =
  emptyResponse s ((hLocation, BSL.toStrict $ BSB.toLazyByteString $ actionURL (Just req) r a (Wai.queryString req)) : h)

otherRouteResponse :: ResponseHeaders -> R.RouteAction r a -> r -> Request -> Response
otherRouteResponse = redirectRouteResponse seeOther303

forbiddenResponse :: RequestContext -> Response
forbiddenResponse = response forbidden403 [] . htmlForbidden

notFoundResponse :: RequestContext -> Response
notFoundResponse = response notFound404 [] . htmlNotFound

maybeAction :: Maybe a -> ActionM a
maybeAction (Just a) = return a
maybeAction Nothing = result =<< peeks notFoundResponse

runActionRoute :: R.RouteMap Action -> Service -> Wai.Application
runActionRoute routeMap routeContext req =
    let eMatchedAction :: Either (Status, ResponseHeaders) Action
        eMatchedAction = R.routeWai req routeMap
        resultingAction :: Action
        resultingAction = either err id eMatchedAction
    in runAction routeContext resultingAction req
  where
    err :: (Status, ResponseHeaders) -> Action
    err (status, headers) = withoutAuth $ peeks $ response status headers . htmlNotFound
