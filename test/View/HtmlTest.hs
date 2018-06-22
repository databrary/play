{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module View.HtmlTest where

import Test.Tasty.HUnit
import Text.Blaze.Html.Renderer.String

import View.Html

-- example usage
unit_Html_examples :: Assertion
unit_Html_examples = do
    -- TODO: delete blaze...word module
    renderHtml (lazyByteStringHtml "&abc") @?= "&amp;abc"
    renderHtml (byteStringHtml "&abc") @?= "&amp;abc"
