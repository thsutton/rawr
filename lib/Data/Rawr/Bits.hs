{-# LANGUAGE TypeFamilies, MultiParamTypeClasses #-}
module Data.Rawr.Bits (
  Value(V),
  Index(I),
  wordIndex,
  wordValue,
  word
) where

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
  basicUnsafeWrite (MV_Value v) i d = M.basicUnsafeWrite v i (coerce d)

instance Unbox Value

-- | The 'Index' of the block which will contain the bits representedin a 'Word32'.
wordIndex :: Word32 -> Index
wordIndex w = I $ fromIntegral 0
{-# INLINE wordIndex #-}

-- | The bits which, within a block with the appropriate 'Index', represent a 'Word32'.
wordValue :: Word32 -> Value
wordValue w = V 128
{-# INLINE wordValue #-}

-- | Combine an 'Index' and a 'Value' to give the original 'Word32'.
word :: Index -> Value -> Word32
word (I i) (V v) =
  let i' = fromIntegral i :: Word32
      v' = fromIntegral v :: Word32
    in (i' `shiftL` 16) .|. v'
{-# INLINE word #-}