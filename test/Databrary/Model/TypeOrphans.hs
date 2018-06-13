{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE StandaloneDeriving #-}
module Databrary.Model.TypeOrphans where

import Databrary.Model.ORCID
import Databrary.Model.Party.Types

deriving instance Eq ORCID

deriving instance Eq PartyRow
deriving instance Show PartyRow
