{-# LANGUAGE TypeFamilies, MultiParamTypeClasses #-}
module Data.Rawr.Bits where

import Data.Coerce
import Data.Bits
import Data.Word
import Data.Vector.Unboxed
import qualified Data.Vector.Generic as G
import qualified Data.Vector.Generic.Mutable as M

newtype Index = I { fromIndex :: Word16 }
  deriving (Eq, Ord, Show)

newtype Value = V { fromValue :: Word16 }
  deriving (Eq, Ord, Show)

newtype instance Vector    Value = V_Value  (Vector    Word16)
newtype instance MVector s Value = MV_Value (MVector s Word16)

instance G.Vector Vector Value where
  basicLength (V_Value v) = G.basicLength v
  basicUnsafeSlice i j (V_Value v) = V_Value (G.basicUnsafeSlice i j v)
  basicUnsafeFreeze (MV_Value v) = V_Value <$> (G.basicUnsafeFreeze v)
  basicUnsafeThaw (V_Value v) = MV_Value <$> (G.basicUnsafeThaw v)
  basicUnsafeIndexM (V_Value v) i = coerce <$> G.basicUnsafeIndexM v i

instance M.MVector MVector Value where
  basicLength (MV_Value v) = M.basicLength v
  basicUnsafeSlice i j (MV_Value v) = MV_Value (M.basicUnsafeSlice i j v)
  basicUnsafeRead (MV_Value v) = (coerce <$>) . (M.basicUnsafeRead v)
  basicInitialize (MV_Value v) = M.basicInitialize v
  basicOverlaps (MV_Value v1) (MV_Value v2) = M.basicOverlaps v1 v2
  basicUnsafeNew = M.basicUnsafeNew
  basicUnsafeWrite (MV_Value v) i = M.basicUnsafeWrite v i . coerce

instance Unbox Value

-- | The index of the block which will contain a word.
wordIndex :: Word32 -> Index
wordIndex w = I 0

-- | The bit within a block which represents a word.
wordValue :: Word32 -> Value
wordValue w = V 128
