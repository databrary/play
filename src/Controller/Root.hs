{-# LANGUAGE CPP, OverloadedStrings #-}
module Controller.Root
  ( viewRoot
  , viewRootHandler
  , viewConstantsHandler
  , viewRobotsTxtHandler
  , notFoundResponseHandler
  ) where

import Control.Monad (when)
import qualified Data.Aeson.Types as JSON
import qualified Data.ByteString as BS
import Data.Maybe (isNothing)
import Data.Text (Text)
import Network.HTTP.Types (notFound404)

import Has
-- import qualified JSON as JSON
import Service.Types
import Action
import Controller.Angular
import View.Root
import Web.Constants
import View.Error (htmlNotFound)

-- TODO: remove when View.Template actionLink replaced
viewRoot :: ActionRoute API
viewRoot = action GET pathAPI $ \api -> viewRootHandler api []

-- NEW HANDLERS
{-
getApiOrFail :: [(BS.ByteString, BS.ByteString)] -> Handler API
getApiOrFail params =
  case params of
    [] -> pure HTML
    ("api", "api"):_ -> pure JSON
    _ -> undefined -- TODO: action m error
-}

-- FIXME: JSON response ignores serviceDown
viewRootHandler :: API -> [(BS.ByteString, BS.ByteString)] -> Action
viewRootHandler api _ = -- TOOD: ensure GET
  withAuth $ do
    down <- peeks serviceDown
    when (api == HTML && isNothing down) angular
    case api of
      JSON -> return $ okResponse [] JSON.emptyObject
      HTML -> peeks $ okResponse [] . maybe htmlRoot htmlDown down

viewConstantsHandler :: [(BS.ByteString, BS.ByteString)] -> Action
viewConstantsHandler _ = -- TODO: ensure GET
  withoutAuth $ return $ okResponse [] $ JSON.pairs constantsJSON

viewRobotsTxtHandler :: [(BS.ByteString, BS.ByteString)] -> Action
viewRobotsTxtHandler _ =  -- TODO: ensure GET
    withoutAuth $ return $ okResponse [] ("" :: Text)
    -- NOTE: DEVEL/SANDBOX behavior wasn't copied here

notFoundResponseHandler :: [(BS.ByteString, BS.ByteString)] -> Action
notFoundResponseHandler _ = withoutAuth $ peeks $ response notFound404 [] . htmlNotFound
