-- |
-- Module:      Data.Roaring
-- Description: Compressed bitmap data structure with good performance.
-- Copyright:   (c) Thomas Sutton 2015
-- License:     BSD3
-- Maintainer:  me@thomas-sutton.id.au
-- Stability:   experimental
--
-- A compressed bitmaps with good space and time performance.
--
-- These modules are intended to be imported qualified, to avoid name clashes
-- with Prelude functions, e.g.
--
-- >  import Data.Roaring (BitMap)
-- >  import qualified Data.Roaring as Roaring
--
-- The implementation paritions values into chunks based on their high 16 bits.
-- Chunks are represented differently based on their density: low-density
-- chunks are stored as packed arrays of the low-order bits while high-density
-- chunks are stored as bit vectors.
--
--    * Samy Chambi, Daniel Lemire, Owen Kaser, Robert Godin,
--    \"/Better bitmap performance with Roaring bitmaps/\", Software: Practice
--    and Experience (to appear) <http://arxiv.org/pdf/1402.6407v4>
--
module Data.Roaring where

import Data.Monoid
import Data.Vector (Vector)
import qualified Data.Vector as V
import Data.Word

import Data.Roaring.Chunk
import Data.Roaring.Utility

-- | A set of bits.
data BitMap = BitMap (Vector Chunk)
  deriving (Show)

type Key = Word32

-- * Query

-- | /O(1)./ Is the set empty?
null :: BitMap -> Bool
null (BitMap v) = V.null v

-- | Cardinality of the set.
size :: BitMap -> Int
size (BitMap cs) = V.sum $ V.map chunkCardinality cs

-- | Is the value a member of the set?
member :: Key -> BitMap -> Bool
member k (BitMap cs) =
    let (i,b) = splitWord k
    in case vLookup (\c -> i == chunkIndex c) cs of
        Nothing    -> False
        Just (_,c) -> chunkGet b c

-- | Is this a subset?
-- @(s1 `isSubsetOf` s2)@ tells whether @s1@ is a subset of @s2.
isSubsetOf :: BitMap -> BitMap -> Bool
isSubsetOf _ _ = False

-- | Is this a proper subset? (i.e. a subset but not equal).
isProperSubsetOf :: BitMap -> BitMap -> Bool
isProperSubsetOf a b = a `isSubsetOf` b && not (b `isSubsetOf` a)

-- | Count the bits set in the range [0,i].
rank :: BitMap -> Int -> Int
rank (BitMap _cs) _ = 0

-- | Find the index of the ith set bit.
select :: BitMap -> Int -> Maybe Key
select (BitMap _cs) _ = Nothing

-- * Construction

-- | /O(1)./ The empty set.
empty :: BitMap
empty = BitMap mempty

-- | /O(1)./ A set of one element.
singleton :: Key -> BitMap
singleton k = insert k empty

-- | Add a value to the set.
insert :: Key -> BitMap -> BitMap
insert k (BitMap v) =
    let (i,b) = splitWord k
        f = Just . maybe (chunkNew i b) (chunkSet b)
        v' = vAlter f (\c -> i == chunkIndex c) v
    in BitMap v'

-- | Delete a value in the set.
--
-- Returns the original set when the value was not present.
delete :: Key -> BitMap -> BitMap
delete k (BitMap v) =
    let (i,b) = splitWord k
        v' = vAlter (f b) (\c -> i == chunkIndex c) v
    in BitMap v'
  where
    f _ Nothing = Nothing
    f b (Just c) =
        let c' = chunkClear b c
        in if 0 == chunkCardinality c'
           then Nothing
           else Just c'

-- * Combine

-- | The union of two sets.
union :: BitMap -> BitMap -> BitMap
union (BitMap cs) (BitMap ds) = BitMap $ mergeWith f cs ds
  where
    f :: Maybe Chunk -> Maybe Chunk -> Maybe Chunk
    f Nothing  b        = b
    f a        Nothing  = a
    f (Just a) (Just b) = Just $ mergeChunks a b

-- | The difference between two sets.
difference :: BitMap -> BitMap -> BitMap
difference _ _ = empty

-- | The intersection of two sets.
intersection :: BitMap -> BitMap -> BitMap
intersection _ _ = empty

-- * Conversion

-- ** List

elems :: BitMap -> [Key]
elems = toAscList

toList :: BitMap -> [Key]
toList = toAscList

fromList :: [Key] -> BitMap
fromList = foldl (flip insert) empty

-- ** Ordered list

-- | Produce a list of 'Key's in a 'BitMap', in descending order.
toAscList :: BitMap -> [Key]
toAscList (BitMap cs) = work cs []
  where
    work cs' l | V.null cs' = l
               | otherwise = let c    = chunkToBits $ V.head cs'
                                 cs'' = V.tail cs'
                             in work cs'' (l <> c)

-- | Produce a list of 'Key's in a 'BitMap', in descending order.
toDescList :: BitMap -> [Key]
toDescList = reverse . toAscList

-- | Build a 'BitMap' from a list of 'Key's.
--
-- Precondition: input is sorted ascending order.
--
-- TODO(thsutton) Throw error if precondition violated.
-- TODO(thsutton) Implement
fromAscList :: [Key] -> BitMap
fromAscList _ = empty

-- * Utility


-- | Merge two 'Vector's of 'Chunk's.
--
-- Precondition: Both vectors are sorted by 'chunkIndex'.
-- Postcondition: Output vector sorted by 'chunkIndex'.
-- Postcondition: length(output) >= max(length(a),length(b))
mergeWith
    :: (Maybe Chunk -> Maybe Chunk -> Maybe Chunk)
    -- ^ Merge two chunks with the same index.
    -> Vector Chunk
    -> Vector Chunk
    -> Vector Chunk
mergeWith f v1 v2
    | V.null v1 = v2
    | V.null v2 = v1
    | otherwise =
        let a = V.head v1
            b = V.head v2
        in work a v1 b v2
  where
    -- Note: we take the head and the *entirety* of each vector; NOT the head
    -- and the tail!
    work :: Chunk -> Vector Chunk -> Chunk -> Vector Chunk -> Vector Chunk
    work a as b bs = case a `compare` b of
        LT -> a `V.cons` mergeWith f (V.tail as) bs
        EQ -> mergeChunks a b `V.cons` mergeWith f (V.tail as) (V.tail bs)
        GT -> b `V.cons` mergeWith f as (V.tail bs)
