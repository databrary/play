{-# LANGUAGE OverloadedStrings #-}
module Controller.Format
  ( viewFormatsHandler
  , formatIcon
  ) where

import Control.Monad.Reader (asks)
import qualified Data.Invertible as I
import Data.Monoid ((<>))
import System.Posix.FilePath (splitFileName, splitExtension)
import qualified Web.Route.Invertible as R

import Model.Format
import Action.Run
import Action
import Controller.Web
import Controller.Angular
import View.Format

-- NOROUTE: Not in routeMapInvertivle; only used for otherRouteResponse
formatIcon :: ActionRoute Format
formatIcon = (pf I.:<->: fp) `R.mapActionRoute` webFile where
  fp f = Just $ staticPath
    [ "images", "filetype", "16px"
    , case formatExtension f of { e:_ -> e ; _ -> "_blank" } <> ".svg"
    ]
  pf (Just (StaticPath p))
    | ("images/filetype/16px/", i) <- splitFileName p
    , (e, ".svg") <- splitExtension i
    , Just f <- getFormatByExtension e = f
  pf _ = unknownFormat

viewFormatsHandler :: Action -- TODO: GET only
viewFormatsHandler = withoutAuth $ do
  angular
  okResponse [] <$> asks htmlFormats
