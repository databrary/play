{-# LANGUAGE OverloadedStrings, ScopedTypeVariables, TypeFamilies #-}
module Model.Id.TypesTest where

import Data.Aeson
import Test.Tasty.HUnit
-- import Test.Tasty

import Model.Id.Types

-- Example using just IdType
data PersonRaw = PersonRaw
    { prId :: IdType PersonRaw
    , prName :: String
    } deriving (Show, Eq)

type instance IdType PersonRaw = Int32

rawPersonId :: IdType PersonRaw
rawPersonId = 1

rawPerson :: PersonRaw
rawPerson =
    PersonRaw
        { prId = 1
        , prName = "Sam"
        }

-- Example using Id and IdType together
data Person = Person
    { prsId :: Id Person
    , prsName :: String
    } deriving (Show, Eq)

type instance IdType Person = Int32

personId :: Id Person
personId = Id 2

person :: Person
person =
    Person
        { prsId = Id 2
        , prsName = "Elias"
        }

unit_toJSON_ID :: Assertion
unit_toJSON_ID =
    -- example
    encode (Id 3 :: Id Person) @?= "3"
