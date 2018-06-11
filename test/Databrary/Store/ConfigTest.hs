{-# LANGUAGE OverloadedStrings #-}
module Databrary.Store.ConfigTest
where

import Data.Aeson hiding (Value)
import qualified Data.ByteString as BS
import Data.Monoid ((<>))
import qualified Data.Text as T
-- import Test.Tasty
import Test.Tasty.HUnit

import Databrary.Store.Config

-- example usage; TODO: replace guts with data.yaml
unit_Config_examples :: Assertion
unit_Config_examples = do
    pathKey (Path ["k1", "k2"]) @?= "k1.k2"
    encode ((mempty :: Config) <> mempty) @?= encode (mempty :: Config)
    mempty ! "k1.k2" @?= (Nothing :: Maybe Value)
    mempty ! "k1.k2" @?= (Nothing :: Maybe ConfigMap)
    encode (mempty ! "k1.k2" :: Maybe Config) @?= encode (Nothing :: Maybe Config)
    mempty ! "k1.k2" @?= (Nothing :: Maybe (Maybe Bool))
    mempty ! "k1.k2" @?= (Nothing :: Maybe Integer)
    mempty ! "k1.k2" @?= (Nothing :: Maybe BS.ByteString)
    mempty ! "k1.k2" @?= (Nothing :: Maybe [Integer])
    mempty ! "k1.k2" @?= (Nothing :: Maybe T.Text)
    mempty ! "k1.k2" @?= (Nothing :: Maybe String)
    mempty ! "k1.k2" @?= (Nothing :: Maybe Int)
