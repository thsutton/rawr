module Data.BitMap.Roaring.Chunk.Low where

import qualified Data.Vector.Algorithms.Heap as S
import qualified Data.Vector.Unboxed         as U
import qualified Data.Vector.Unboxed.Mutable as M
import           Data.Word

import Data.BitMap.Roaring.Utility

-- | "Low density" bits stored in a vector.
newtype LDVector = LDVector { unwrapVector :: U.Vector Word16 }
  deriving (Eq, Show)

-- * Constructors

-- | An empty low-density vector.
empty :: LDVector
empty = LDVector U.empty

-- | A singleton low-density vector.
singleton :: Word16 -> LDVector
singleton b = LDVector (U.singleton b)

-- * Modifying vectors

-- | Set a word in a low-density vector.
--
-- TODO: Implement in O(log n).
setBit :: LDVector -> Word16 -> LDVector
setBit lv@(LDVector bs) ix
    | testBit lv ix = lv
    | otherwise = LDVector (U.modify (S.sortBy (compare)) $ U.cons ix bs)

-- | Clear a word in a low-density vector.
--
-- Pre-condition: The word is present in the vector.
-- Post-condition: The word is not present in the vector.
--
-- TODO: Implement in O(log n)
clearBit :: LDVector -> Word16 -> LDVector
clearBit (LDVector bs) ix =
  LDVector (U.filter (/= ix) bs)

-- | Flip a bit in a low-density vector.
--
-- TODO Implement in O(log n)
complementBit :: LDVector -> Word16 -> LDVector
complementBit lv ix
  | testBit lv ix = clearBit lv ix
  | otherwise = setBit lv ix

-- * Queries

-- | Check whether a word is present in a low-density vector.
--
-- TODO: Implement in O(log n)
testBit :: LDVector -> Word16 -> Bool
testBit (LDVector bs) ix = U.elem ix bs

-- | Query the number of set bits in a low-density vector.
popCount :: LDVector -> Int
popCount (LDVector v) = U.length v

-- * Operators

-- | Take the intersection of two low-density vectors.
intersection :: LDVector -> LDVector -> LDVector
intersection (LDVector as) (LDVector bs) =
    LDVector (vMergeWith merge as bs)
  where
    merge (Just a) (Just b) = Just a
    merge (Just a) Nothing = Nothing
    merge Nothing (Just b) = Nothing
    merge Nothing Nothing = Nothing

-- | Take the union of two low-density vectors.
--
-- NOTE: Callers must check and enforce the density invariant.
union :: LDVector -> LDVector -> LDVector
union (LDVector v1) (LDVector v2) =
  LDVector (vMergeWith merge v1 v2)
  where
    merge (Just a) (Just b) = Just a
    merge Nothing a = a
    merge a Nothing = a

-- | Take the exclusive-or of two low-density vectors.
xor :: LDVector -> LDVector -> LDVector
xor (LDVector as) (LDVector bs) =
    LDVector (vMergeWith merge as bs)
  where
    merge a Nothing = a
    merge Nothing b = b
    merge _ _ = Nothing

-- * Conversions

-- | Unpack the 'Word16's set in a 'HDVector'.
toList :: LDVector -> [Word16]
toList (LDVector v) = U.toList v
