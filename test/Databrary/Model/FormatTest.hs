{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module Databrary.Model.FormatTest where

-- import Data.Maybe
-- import Hedgehog
-- import qualified Hedgehog.Gen as Gen
-- import qualified Hedgehog.Range as Range
-- import Test.Tasty
import Test.Tasty.HUnit

import Databrary.JSON
import Databrary.Model.Format
-- import Databrary.Model.Id

unit_mimeTypeTop :: Assertion
unit_mimeTypeTop = do
    -- example
    mimeTypeTop "text/plain" @?= "text"

unit_formatJSON :: Assertion
unit_formatJSON = do
    -- example
    (recordObject . formatJSON) videoFormat
        @?=
           ([("id",Number (-800.0))
            ,("mimetype",String "video/mp4")
            ,("extension",String "mp4")
            ,("name",String "MPEG-4 video")
            ,("transcodable",Number (-800.0))]
            :: [Pair])

