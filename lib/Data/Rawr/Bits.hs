module Data.Rawr.Bits where

import Data.Coerce
import Data.Bits
import Data.Word

newtype Index = I { fromIndex :: Word16 }
  deriving (Eq, Ord, Show)

newtype Value = V { fromValue :: Word16 }
  deriving (Eq, Ord, Show)

-- | The index of the block which will contain a word.
wordIndex :: Word32 -> Index
wordIndex w = I 0

-- | The bit within a block which represents a word.
wordValue :: Word32 -> Value
wordValue w = V 128
