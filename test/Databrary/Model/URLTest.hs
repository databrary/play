{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module Databrary.Model.URLTest where

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

-- hdlURL

unit_parseURL :: Assertion
unit_parseURL = do
    -- example
    parseURL "http://google.com" @?= parseURI "http://google.com"
    -- typical
    parseURL "10.1000/xyz123" @?= parseURI "hdl:10.1000/xyz123"
    parseURL "http://doi.org/10.1000/xyz123" @?= parseURI "hdl:10.1000/xyz123"

unit_urlLink :: Assertion
unit_urlLink = do
    -- example
    (fmap urlLink . parseURI) "hdl:rest" @?= parseURI "http://hdl.handle.net/rest"
