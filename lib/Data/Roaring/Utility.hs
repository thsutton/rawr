module Data.Roaring.Utility where

import Data.Bits
import Data.Convertible
import Data.Monoid
import qualified Data.Vector as V
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

-- | Alter the 'Chunk' with the given index in a vector of 'Chunk's.
--
-- The function is passed 'Nothing' if the 'Chunk' is not present.
--
-- If the function returns 'Nothing' the 'Chunk', if present, is deleted;
-- otherwise it is replaced.
vAlter
    :: Ord a
    => (Maybe a -> Maybe a )
    -> (a -> Bool)
    -> V.Vector a
    -> V.Vector a
vAlter f p v = case vLookup p v of
    Nothing     -> case f Nothing of
        Nothing -> v
        Just c' -> vInsert v c' -- TODO(thsutton) Insert
    Just (i, a) -> case f (Just a) of
        Nothing -> vDelete v i
        Just c' -> V.update v (V.singleton (i, c'))

-- | Search for a 'Chunk' with a specific index.
--
-- TODO(thsutton) better search algorithm.
vLookup :: Ord a => (a -> Bool) -> V.Vector a -> Maybe (Int, a)
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
vInsert :: Ord a => V.Vector a -> a -> V.Vector a
vInsert v a =
    if a `V.elem` v
    then v
    else V.modify VAH.sort $ V.cons a v

-- | Delete the element at index.
--
-- Return the vector unchanged if the index is out of bounds.
vDelete :: Ord a => V.Vector a -> Int -> V.Vector a
vDelete v p
    | p < 0 = v
    | V.length v < p = v
    | otherwise = case V.splitAt p v of
        (s, r) -> s <> V.tail r

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

-- | Delete an element from an unboxed vector.
--
-- Precondition: input vector is sorted.
-- Postcondition: output vector is sorted.
-- Postcondition: not $ a `elem` v'
--
-- TODO(thsutton): Efficiency.
uvDelete :: (U.Unbox a, Ord a) => U.Vector a -> a -> U.Vector a
uvDelete v a = case U.elemIndex a v of
    Nothing -> v
    Just p  -> case U.splitAt p v of
        (s, r) -> s <> U.tail r
