module JSONTest where

-- import Data.Text
import Data.Aeson.Types
import qualified Data.Either as E
import Data.Monoid ((<>))
import qualified Data.HashMap.Strict as HM
import qualified Data.Vector as V
import Test.Tasty.HUnit

import JSON

unit_JSON_examples :: Assertion
unit_JSON_examples = do
  "k1" .= (Nothing :: Maybe Int) @?= ([("k1" , Null)] :: [Pair])
  "k1" .= (Nothing :: Maybe Int) @?= (HM.singleton "k1" Null :: Object)
  -- TOJSON unsafeencoding
  -- ToNestedObject, .=.
  "k1" `kvObjectOrEmpty` (Nothing :: Maybe Int) @?= ([] :: [Pair])
  "k1" `kvObjectOrEmpty` (Just 3 :: Maybe Int) @?= ([("k1", Number 3)] :: [Pair])
  let r1 = Record (1 :: Int) (("k1" .= Null) <> ("k2" .= Null) :: [Pair])
  let o1 = ("k3" .= Null) :: [Pair]
  recordObject (foldObjectIntoRec r1 o1)
      @?= (("id" .= (1 :: Int)) <> ("k1" .= Null) <> ("k2" .= Null) <> ("k3" .= Null))
  recordObject r1 @?= ("id" .= (1 :: Int)) <> ("k1" .= Null) <> ("k2" .= Null)
  let r2 = Record (1 :: Int) (("k1" .= Null) <> ("k2" .= Null) :: Series)
  recordEncoding r2 @?= pairs (("id" .= (1 :: Int)) <> ("k1" .= Null) <> ("k2" .= Null))
  -- TODO: mapRecords
  -- TODO: .=:
  recordMap [Record (1 :: Int) ("k1" .= Null :: [Pair])]
      @?= ([("1", object [("k1",Null),("id",Number 1.0)])] :: [Pair])
  E.isLeft (parseEither ((`lookupAtParse` 3) :: Array -> Parser String) (V.fromList [Null, Null]))
      @? "lookupAtParse fails on idx too large"
  -- TODO: tojson, fromjson bytestring, html
  -- TODO: jsonquery
