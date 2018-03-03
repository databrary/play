{-# LANGUAGE CPP, OverloadedStrings #-}
module Databrary.Controller.Root
  ( viewRoot
  , viewConstants
  , viewRobotsTxtHandler
  ) where

import Control.Monad (when)
import qualified Data.Aeson.Types as JSON
import qualified Data.ByteString as BS
import Data.Maybe (isNothing)
import qualified Data.Text as T
import Data.Text (Text)

import Databrary.Has
import qualified Databrary.JSON as JSON
import Databrary.Service.Types
import Databrary.HTTP.Path.Parser
import Databrary.Action
import Databrary.Controller.Angular
import Databrary.View.Root
import Databrary.Web.Constants

viewRoot :: ActionRoute API
viewRoot = action GET pathAPI $ \api -> withAuth $ do
  down <- peeks serviceDown
  when (api == HTML && isNothing down) angular
  case api of
    JSON -> return $ okResponse [] JSON.emptyObject
    HTML -> peeks $ okResponse [] . maybe htmlRoot htmlDown down

viewConstants :: ActionRoute ()
viewConstants = action GET (pathJSON >/> "constants") $ \() -> withoutAuth $
  return $ okResponse [] $ JSON.objectEncoding constantsJSON

-- NEW HANDLERS 

viewRobotsTxtHandler :: [(BS.ByteString, BS.ByteString)] -> Action
viewRobotsTxtHandler [] =  -- TODO: ensure GET
    withoutAuth $ return $ okResponse [] ("" :: Text)
    -- NOTE: DEVEL/SANDBOX behavior wasn't copied here
