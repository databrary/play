{-# LANGUAGE OverloadedStrings, ScopedTypeVariables, TypeFamilies #-}
module Databrary.Model.Id.TypesTest where

-- import Test.Tasty.HUnit
-- import Test.Tasty

import Databrary.Model.Id.Types


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
