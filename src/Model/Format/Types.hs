{-# LANGUAGE TemplateHaskell, OverloadedStrings, TypeFamilies #-}
module Model.Format.Types
  ( Format(..)
  , unknownFormat
  ) where

import qualified Data.ByteString as BS
import Data.Function (on)
import Data.Int (Int16)
import Data.Ord (comparing)
import qualified Data.Text as T
import Instances.TH.Lift ()
import Language.Haskell.TH.Lift (deriveLift)

import Model.Kind
import Model.Id.Types

type instance IdType Format = Int16

data Format = Format
  { formatId :: Id Format
  , formatMimeType :: BS.ByteString
  , formatExtension :: [BS.ByteString] -- TODO: nonempty list
  , formatName :: T.Text
  }
  -- deriving (Show)

instance Kinded Format where
  kindOf _ = "format"

instance Eq Format where
  (==) = on (==) formatId
  (/=) = on (/=) formatId

instance Ord Format where
  compare = comparing formatId

deriveLift ''Format

unknownFormat :: Format
unknownFormat = Format
  { formatId = error "unknownFormat"
  , formatMimeType = "application/octet-stream"
  , formatExtension = []
  , formatName = "Unknown"
  }

