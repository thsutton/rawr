-- |
module Data.BitMap.Roaring.Chunk.High where

import           Data.Bits                   ((.&.), (.|.))
import qualified Data.Bits                   as B
import qualified Data.Vector.Algorithms.Heap as S
import qualified Data.Vector.Unboxed         as U
import qualified Data.Vector.Unboxed.Mutable as M
import           Data.Word

-- | "High density" bits packed into a vector.
newtype HDVector = HDVector (U.Vector Word64)
  deriving (Eq, Show)

-- * Construct

-- | Empty high-density vector.
empty :: HDVector
empty =
  HDVector (U.replicate 1024 0)

-- | New high-density vector with a single bit set.
singleton :: Word16 -> HDVector
singleton i =
    let (wi, bi) = fromIntegral i `divMod` 64
        mk i = if i == wi
          then B.bit bi
          else 0
    in HDVector (U.generate 1024 mk)

-- * Modify

-- | Set a bit in a high-density vector.
setBit :: HDVector -> Word16 -> HDVector
setBit (HDVector bs) ix =
    let (w, b) = fromIntegral ix `divMod` 64
    in HDVector (U.modify (\v -> M.read v w >>= M.write v w . flip B.setBit b) bs)

-- | Clear a bit in a high-density vector.
clearBit :: HDVector -> Word16 -> HDVector
clearBit (HDVector bs) ix =
    let (w, b) = fromIntegral ix `divMod` 64
    in HDVector (U.modify (\v -> M.read v w >>= M.write v w . flip B.clearBit b) bs)

-- | Flip a bit in a high-density vector.
--
-- TODO Specialise implementation.
complementBit :: HDVector -> Word16 -> HDVector
complementBit v ix
  | testBit v ix = clearBit v ix
  | otherwise = setBit v ix

-- * Operators

-- | Take the intersection of two high-density vectors.
--
-- NOTE: Callers must check and enforce the density invariant.
intersection :: HDVector -> HDVector -> HDVector
intersection (HDVector as) (HDVector bs) =
  HDVector (U.zipWith (.&.) as bs)

-- | Take the union of two high-density vectors.
union :: HDVector -> HDVector -> HDVector
union (HDVector as) (HDVector bs) =
  HDVector (U.zipWith (.|.) as bs)

-- | Take the exclusive or of two high-density vectors.
--
-- NOTE: Callers must check and enforce the density invariant.
xor :: HDVector -> HDVector -> HDVector
xor (HDVector as) (HDVector bs) =
  HDVector (U.zipWith B.xor as bs)

-- * Query

-- | Test a bit in a high-density vector.
testBit :: HDVector -> Word16 -> Bool
testBit (HDVector bs) ix =
    let (wi, bi) = fromIntegral ix `divMod` 64
    in B.testBit (bs U.! wi) bi

-- | Query number of set bits.
popCount :: HDVector -> Int
popCount (HDVector v) = U.foldl' (\a b -> a + B.popCount b) 0 v

-- * Conversions

-- | Unpack the 'Word16's set in a 'HDVector'.
toList :: HDVector -> [Word16]
toList (HDVector v) = []
  where
    unpackWord :: Word64 -> [Word16]
    unpackWord w = fmap fst . filter snd $ fmap (\bi -> (fromIntegral bi, B.testBit w bi)) [0..63]

