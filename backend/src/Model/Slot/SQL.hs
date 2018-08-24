{-# LANGUAGE TemplateHaskell #-}
module Model.Slot.SQL
  ( selectSlotId
  , slotKeys
  ) where

import Model.SQL.Select
import Model.Slot.Types

selectSlotId :: String -> Selector
selectSlotId table = selectColumns 'SlotId table ["container", "segment"]

slotKeys :: String -- ^ @'Slot'@
  -> [(String, String)]
slotKeys o =
  [ ("container", "${containerId $ containerRow $ slotContainer " ++ o ++ "}")
  , ("segment", "${slotSegment " ++ o ++ "}")
  ]
