{-# LANGUAGE FlexibleContexts #-}
module Data.Rawr.Dense where

import           Data.Bits ((.&.), (.|.), shiftR)
import qualified Data.Bits as B
import           Data.Vector.Unboxed (Vector)
import qualified Data.Vector.Generic as V
import           Data.Word

import Data.Rawr.Bits

-- | A Vector of 'Value's stored as 2^16 bits.
newtype DenseVector = DV (Vector Word64)
  deriving (Eq, Ord)

-- | 1024 * 64 = 2 ^ 16 bits.
vector_length :: Int
vector_length = 1024

empty :: DenseVector
empty = DV (V.replicate vector_length 0)

toList :: DenseVector -> [Value]
toList (DV v) = []

pack :: V.Vector v Value => v Value -> DenseVector
pack = V.foldl setBit empty

-- | 'Vector Word64' index which corresponsed to a value.
--
-- This is just the high 10 bits of the value.
valueIndex :: Value -> Int
valueIndex (V v) = fromIntegral (v `shiftR` 6)

-- | 'Word64' bit which corresponds to a value.
--
-- This is just the low 6 bits.
valueBit :: Value -> Int
valueBit (V v) = fromIntegral (v .&. 63)

testBit :: DenseVector -> Value -> Bool
testBit (DV d) v =
  let w = valueIndex v
      b = valueBit v
  in (d V.! w) `B.testBit` b

setBit :: DenseVector -> Value -> DenseVector
setBit (DV d) v =
  let wix = valueIndex v
      bix = valueBit v
      w = (d V.! wix) `B.setBit` bix
  in DV $ V.unsafeUpdate d (V.singleton (wix, w))

clearBit :: DenseVector -> Value -> DenseVector
clearBit (DV d) v =
  let wix = valueIndex v
      bix = valueBit v
      w = (d V.! wix) `B.clearBit` bix
  in DV $ V.unsafeUpdate d (V.singleton (wix, w))
