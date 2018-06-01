{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

-- | Utilities for serving a Servant API with Action handlers.
module Databrary.Action.Servant
    (
    -- * Building the application
      servantApp
    ) where

import Servant

import Databrary.API
import Databrary.Action (ActionRouteApp (..))

-- | Create a server to serve the ServantAPI.
apiServer :: ActionRouteApp -> Server ServantAPI
apiServer (ActionRouteApp app) = preferAngularServer :<|> Tagged app

preferAngularServer :: Server PreferAngularAPI
preferAngularServer _ _ = emptyServer
-- Up next: Serve routes that prefer Angular.
-- preferAngularServer :: Server PreferAngularAPI
-- preferAngularServer u j =
--     -- userLogin
--     :<|> foo
--     :<|> bar
--   where
--     angular' = preferAngular u j
--     userLogin = angular' (pure "no angular, no")
--
-- enableAngular :: Maybe Text -> Maybe Int -> Bool
-- enableAngular _ jsOpt = maybe True (/= 0) jsOpt
--
-- preferAngular mUserAgent mJsParam fallback =
--     if enableAngular mUserAgent mJsParam
--     then pure "I am an Angular!"
--     else fallback

-- | Boilerplate. This will go away when Servant adopts type application.
servantAPI :: Proxy ServantAPI
servantAPI = Proxy

-- | The lowest level of the Databrary 'web framework'. It simply builds a Wai
-- Application out of the 'apiServer' description.
--
-- Since Servant does not control most routes yet, this function delegates the
-- rest to the original ActionRoute-based app.
servantApp :: ActionRouteApp -> Application
servantApp arApp =
    serve servantAPI (apiServer arApp)
