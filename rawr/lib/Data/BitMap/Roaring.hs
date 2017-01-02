-- |
-- Module:      Data.BitMap.Roaring
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
-- >  import           Data.BitMap.Roaring (BitMap)
-- >  import qualified Data.BitMap.Roaring as Roaring
--
-- The implementation partitions values into chunks based on their
-- high 16 bits. Chunks are represented according to their density:
-- low-density chunks are stored as packed arrays of the low-order
-- bits while high-density chunks are stored as bit vectors.
--
--    * Samy Chambi, Daniel Lemire, Owen Kaser, Robert Godin,
--    \"/Better bitmap performance with Roaring bitmaps/\", Software: Practice
--    and Experience (to appear) <http://arxiv.org/pdf/1402.6407v4>
--
module Data.BitMap.Roaring where

import           Data.Bits
import           Data.Monoid
import           Data.Vector (Vector)
import qualified Data.Vector as V
import           Data.Word

import           Data.BitMap.Roaring.Chunk   (Chunk)
import qualified Data.BitMap.Roaring.Chunk   as C
import           Data.BitMap.Roaring.Utility

-- | A set of bits.
data BitMap = BitMap (Vector Chunk)
  deriving (Show, Eq)

type Key = Word32

instance Bits BitMap where
  bitSize _ = 2^32
  bitSizeMaybe _ = Just (2^32)
  isSigned _ = False

  (.&.) = intersection
  (.|.) = union
  xor = const -- TODO
  complement a = a

  shift x i = x
  rotate x i = x

  zeroBits = BitMap V.empty
  bit i = singleton (fromIntegral i)

  popCount x = 0
  testBit x i = False
  setBit x i = x
  clearBit x i = x
  complementBit x i = x

instance FiniteBits BitMap where
  finiteBitSize _ = 2^32

-- * Query

-- | /O(1)./ Is the set empty?
null :: BitMap -> Bool
null (BitMap v) = V.null v

-- | Cardinality of the set.
size :: BitMap -> Int
size (BitMap cs) = V.foldl' (\s c-> s + popCount c) 0 cs

-- | Is the value a member of the set?
member :: Key -> BitMap -> Bool
member k (BitMap cs) =
    let (i,b) = splitWord k
    in case vLookup (\c -> i == C.chunkIndex c) cs of
        Nothing     -> False
        Just (_, c) -> C.chunkCheck b c

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
        f = Just . maybe (C.chunkNew i b) (C.chunkSet b)
        v' = vAlter f (\c -> i == C.chunkIndex c) v
    in BitMap v'

-- | Delete a value in the set.
--
-- Returns the original set when the value was not present.
delete :: Key -> BitMap -> BitMap
delete k (BitMap v) =
    let (i,b) = splitWord k
        v' = vAlter (f b) (\c -> i == C.chunkIndex c) v
    in BitMap v'
  where
    f _ Nothing = Nothing
    f b (Just c) =
        let c' = C.chunkClear b c
        in if 0 == popCount c'
           then Nothing
           else Just c'

-- * Combine

-- | The union of two sets.
union :: BitMap -> BitMap -> BitMap
union (BitMap cs) (BitMap ds) =
    BitMap (vMergeWith merge cs ds)
  where
    merge :: Maybe Chunk -> Maybe Chunk -> Maybe Chunk
    merge (Just a) (Just b) =
      let c = a `C.union` b
      in if C.null c then Nothing else Just c
    merge (Just a) Nothing = Just a
    merge Nothing (Just b) = Just b
    merge Nothing Nothing = Nothing

-- | The intersection of two sets.
intersection :: BitMap -> BitMap -> BitMap
intersection (BitMap as) (BitMap bs) =
    BitMap (vMergeWith merge as bs)
  where
    merge (Just a) (Just b) =
      let c = a `C.intersection` b
      in if C.null c then Nothing else Just c
    merge _ _ = Nothing

-- | The difference between two sets.
difference :: BitMap -> BitMap -> BitMap
difference (BitMap as) (BitMap bs) =
    BitMap (vMergeWith merge as bs)
  where
    merge _ _ = Nothing

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
               | otherwise = let c    = C.toList $ V.head cs'
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
