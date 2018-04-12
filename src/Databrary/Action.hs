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

import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as BSB
import qualified Data.ByteString.Lazy as BSL
import Network.HTTP.Types (Status, seeOther303, forbidden403, notFound404, ResponseHeaders, hLocation)
import qualified Network.Wai as Wai
import qualified Web.Route.Invertible.Wai as R
import qualified Network.Wai.Route as WAR
import Servant.API
import Servant

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

type API1 = Raw

api1 :: Proxy API1
api1 = Proxy

serverApi1 :: [(BS.ByteString, WAR.Handler IO)] -> Server API1
serverApi1 newRouteMap =
    Tagged (WAR.route newRouteMap)

api1App :: [(BS.ByteString, WAR.Handler IO)] -> Application
api1App newRouteMap =
    serve api1 (serverApi1 newRouteMap)

runActionRoute
  :: R.RouteMap Action -> (Service -> [(BS.ByteString, WAR.Handler IO)]) -> Service -> Wai.Application
runActionRoute routeMap mkNewRouteMap routeContext req =
    let eMatchedAction :: Either (Status, ResponseHeaders) Action
        eMatchedAction = R.routeWai req routeMap
    in
      case eMatchedAction of
        Right act ->
            runAction routeContext act req
        Left (st,hdrs) ->
            if st == notFound404 -- currently, this might be only possible error result?
            then
                api1App (mkNewRouteMap routeContext) req
            else
                runAction routeContext (err (st,hdrs)) req
  where
    err :: (Status, ResponseHeaders) -> Action
    err (status, headers) = withoutAuth $ peeks $ response status headers . htmlNotFound

-- TODO: delete notes below
-- route
--  :: Monad m => [(ByteString, Handler m)] -> Request -> (Response -> m ResponseReceived) -> m ResponseReceived
{-
type Handler m
= [(ByteString, ByteString)] -- The captured path parameters.
-> Request -- The matched Request.
-> (Response -> m ResponseReceived) -- The continuation.
-> m ResponseReceived
-}

{-
type ActionRoute a = R.RouteAction a Action

data Action = Action
  { _actionAuth :: !Bool
  , _actionM :: !(ActionM Response)
  }
newtype ActionM a = ActionM { unActionM :: ReaderT RequestContext IO a }
  deriving (Functor, Applicative, Alternative, Monad, MonadPlus, MonadIO, MonadBase IO, MonadThrow, MonadReader RequestContext)
-}
