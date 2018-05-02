{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module Databrary.Model.URLTest where

import Data.Maybe
import Network.URI
import Test.Tasty.HUnit

import Databrary.Model.URL

unit_validHDL :: Assertion
unit_validHDL = do
    -- example
    validHDL "120.1/" @? "[0-9]+(.[0-9]+)+/.+ is valid"
    -- typical
    not (validHDL "a") @? "HDL should start with a digit"
    -- edge case
    validHDL "0.0.0/" @? "digits and dots ending with slash is okay"

unit_parseURL :: Assertion
unit_parseURL = do
    -- example
    parseURL "http://google.com" @?= parseURI "http://google.com"

unit_urlLink :: Assertion
unit_urlLink = do
    -- example
    (urlLink . fromJust . parseURI) "hdl:rest" @?= (fromJust . parseURI) "http://hdl.handle.net/rest"
