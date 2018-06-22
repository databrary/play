{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
-- | This module exists purely to break a cycle between Action and
-- Routes. I would like to see its contents folded into
-- Routes eventually, if possible.
module API where

import Data.Text (Text)
import Servant
-- Up next:
-- import Servant.HTML.Blaze (HTML)
-- import Text.Blaze.Html (Html)

-- | The Databrary API, as described by Servant. It's called ServantAPI, instead
-- of DatabraryAPI, because it's currently the *third* API description for
-- Databrary in this codebase.
type ServantAPI
    = PreferAngularAPI
    :<|> ActionRouteAPI

-- The fallback to OG routing
type ActionRouteAPI = Raw

-- | Routes that fire up the Angular SPA if possible.
type PreferAngularAPI
    = Header "User-Agent" Text :> QueryParam "js" Int :> EmptyAPI
        -- Up next:
        -- "user" :> "login" :> Get '[HTML] Html
