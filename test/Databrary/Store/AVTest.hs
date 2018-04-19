{-# LANGUAGE OverloadedStrings #-}

module Databrary.Store.AVTest where

import Test.Tasty
import Test.Tasty.HUnit
import qualified Data.ByteString.Char8 as Char8

import Paths_databrary (getDataFileName)
import Databrary.Store.AV

test_all :: [TestTree]
test_all =
    [ testCase "sanity" $ do
        filename <- Char8.pack <$> getDataFileName "test/data/small.webm"
        prb      <- avProbe filename =<< initAV
        avProbeFormat prb @?= "matroska,webm"
        avProbeStreams prb
            @?= [(AVMediaTypeVideo, "vp8"), (AVMediaTypeAudio, "vorbis")]
    ]
