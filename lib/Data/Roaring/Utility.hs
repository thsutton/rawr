
{-# LANGUAGE FlexibleContexts #-}
module Data.Roaring.Utility where

import Data.Bits
import Data.Convertible
import Data.Monoid
import qualified Data.Vector.Generic as V
import qualified Data.Vector.Algorithms.Heap as VAH
import qualified Data.Vector.Unboxed as U
import Data.Word

-- * Words

-- | Split a 'Word32' into its high-order and low-order bits.
splitWord :: Word32 -> (Word16, Word16)
splitWord w =
    let h = convert $ rotate (0xffff0000 .&. w) 16
        l = convert $ 0x0000ffff .&. w
    in (h,l)

-- | Combine two 'Word16's of the high-order and low-order bits into a
-- 'Word32'.
combineWord :: Word16 -> Word16 -> Word32
combineWord h l = rotate (convert h) (-16) .|. convert l

-- * Vectors

vMerge :: (U.Unbox e, Ord e) => U.Vector e -> U.Vector e -> U.Vector e
vMerge as bs
    | U.null as = bs
    | U.null bs = as
    | otherwise =
        let a = U.head as
            b = U.head bs
        in case a `compare` b of
            LT -> a `U.cons` vMerge (U.tail as) bs
            EQ -> a `U.cons` vMerge (U.tail as) (U.tail bs)
            GT -> b `U.cons` vMerge as (U.tail bs)

-- | Alter a value in a vector.
--
--   Modify the value at an index, if present. If the function returns
--   'Just v', the item will be replaced with v.
vAlter
    :: (V.Vector v a)
    => (a -> Maybe a)
    -> Int
    -> v a
    -> v a
vAlter f ix v =
    case v V.!? ix >>= f of
        Just x' -> v V.// [(ix, x')]
        Nothing -> v

-- | Alter the 'Chunk' with the given index in a vector of 'Chunk's.
--
-- The function is passed 'Nothing' if the 'Chunk' is not present.
--
-- If the function returns 'Nothing' the 'Chunk', if present, is deleted;
-- otherwise it is replaced.
vAlterWith
    :: (V.Vector v a, V.Vector v (Int, a), Ord a)
    => (Maybe a -> Maybe a )
    -> (a -> Bool)
    -> v a
    -> v a
vAlterWith f p v = case vLookup p v of
    Nothing     -> case f Nothing of
        Nothing -> v
        Just c' -> vInsert v c' -- TODO(thsutton) Insert
    Just (i, a) -> case f (Just a) of
        Nothing -> vDelete v i
        Just c' -> V.update v (V.singleton (i, c'))

-- | Search for a 'Chunk' with a specific index.
--
-- TODO(thsutton) better search algorithm.
vLookup
    :: (V.Vector v a, Ord a)
    => (a -> Bool)
    -> v a
    -> Maybe (Int, a)
vLookup p v = case V.findIndex p v of
    Nothing -> Nothing
    Just i  -> Just (i, v V.! i)

-- | Insert a 'Chunk' into a vector, replacing the
--
-- Precondition: input vector is sorted.
-- Postcondition: output vector is sorted.
-- Postcondition: a `elem` v'.
--
-- TODO(thsutton): Efficiency.
vInsert
    :: (V.Vector v a, Ord a)
    => v a
    -> a
    -> v a
vInsert v a =
    if a `V.elem` v
    then v
    else V.modify VAH.sort $ V.cons a v

-- | Delete the element at index.
--
-- Return the vector unchanged if the index is out of bounds.
vDelete
    :: (V.Vector v a, Ord a)
    => v a
    -> Int
    -> v a
vDelete v p
    | p < 0 = v
    | V.length v < p = v
    | otherwise = case V.splitAt p v of
        (s, r) -> s V.++ V.tail r

-- * Unboxed Vectors

-- | Insert a 'Chunk' into an unboxed vector.
--
-- Precondition: input vector is sorted.
-- Postcondition: output vector is sorted.
-- Postcondition: a `elem` v'.
--
-- TODO(thsutton): Efficiency.
uvInsert :: (U.Unbox a, Ord a) => U.Vector a -> a -> U.Vector a
uvInsert v a =
    if a `U.elem` v
    then v
    else U.modify VAH.sort $ U.cons a v
