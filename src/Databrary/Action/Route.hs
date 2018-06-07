{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}
module Databrary.Action.Route
  ( Method(..)
  , ActionRoute
  , actionURL
  , actionURI
  , actionMethod
  , action
  , multipartAction
  , API(..)
  , pathHTML
  , pathJSON
  , pathAPI
  ) where

import qualified Data.ByteString.Builder as BSB
import qualified Data.Invertible as I
import Network.HTTP.Types (Query)
import Network.URI (URI(..))
import qualified Web.Route.Invertible as R
import Web.Route.Invertible (Method(..))

import Databrary.HTTP.Request
import Databrary.HTTP.Route
import Databrary.HTTP.Path.Parser
import Databrary.Action.Run

-- | A 'R.RouteAction' (library code) that holds an 'Action' (Databrary code).
-- The type parameter a represents the values that get captured in a route
-- description (like /foo/:int would capture an Int).
type ActionRoute a = R.RouteAction a Action

actionURL :: Maybe Request -> R.RouteAction r a -> r -> Query -> BSB.Builder
actionURL mreq route routeParams query
  | R.requestMethod rr == GET = routeURL mreq rr query
  | otherwise = error $ "actionURL: " ++ show rr
  where rr = R.requestActionRoute route routeParams

actionURI :: Maybe Request -> R.RouteAction r a -> r -> Query -> URI
actionURI req r a q
  | R.requestMethod rr == GET = routeURI req rr q
  | otherwise = error $ "actionURI: " ++ show rr
  where rr = R.requestActionRoute r a

actionMethod :: R.RouteAction r a -> r -> Method
actionMethod r = R.requestMethod . R.requestActionRoute r

-- | A shortcut for specifying route actions.
action
    :: Method -- ^ HTTP method to handle
    -> PathParser r -- ^ Path to handle (r holds the captured elements)
    -> (r -> a) -- ^ Action to build the response (a)
    -> R.RouteAction r a -- ^ The complete, built route/action specifier.
action method path act =
    R.routePath path R.>* R.routeMethod method `R.RouteAction` act

multipartAction :: R.RouteAction q a -> R.RouteAction q a
multipartAction (R.RouteAction r a) =
  R.RouteAction (r R.>* (R.routeAccept "multipart/form-data" R.>| R.unit)) a

data API
  = HTML
  | JSON
  deriving (Eq)

pathHTML :: PathParser ()
pathHTML = R.unit

pathJSON :: PathParser ()
pathJSON = "api"

pathAPI :: PathParser API
pathAPI = [I.biCase|Left () <-> JSON ; Right () <-> HTML|] R.>$< (pathJSON R.>|< pathHTML)
