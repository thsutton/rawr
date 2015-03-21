-- |
-- Module: Data.BitMap.Roaring
-- Description: Compressed bitmap data structure with good performance.
-- Copyright: (c) Thomas Sutton 2015
-- License: BSD3
-- Maintainer: me@thomas-sutton.id.au
-- Stability: experimental
--
-- A compressed bitmaps with good space and time performance.
--
-- These modules are intended to be imported qualified, to avoid name clashes
-- with Prelude functions, e.g.
--
-- >  import Data.BitMap.Roaring (BitMap)
-- >  import qualified Data.BitMap.Roaring as Roaring
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
module Data.BitMap.Roaring where

import Data.Bits
import Data.Convertible
import Data.Monoid
import Data.Vector (Vector)
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U
import Data.Word

-- | A set of bits.
data BitMap = BitMap (Vector Chunk)

type Key = Word32

-- | A chunk representing the keys which share particular 16 high-order bits.
--
-- Chunk with low density (i.e. no more than 4096 members) are represented as a
-- sorted array of their low 16 bits. Chunks with high density (i.e. more than
-- 4096 members) are represented by a bit vector.
--
-- Both high and low density chunks include the high order bits shared by all
-- entries in the chunk, and the cardinality of the chunk.
data Chunk
    = LowDensity
        { chunkIndex       :: Word16
        , chunkCardinality :: Int
        , chunkArray       :: U.Vector Word16
        }
    | HighDensity
        { chunkIndex       :: Word16
        , chunkCardinality :: Int
        , chunkBits        :: U.Vector Word64
        }

-- * Query

-- | Is the set empty?
null :: BitMap -> Bool
null (BitMap v) = V.null v

-- | Cardinality of the set.
size :: BitMap -> Int
size _ = 0

-- | Is the value a member of the set?
member :: Key -> BitMap -> Bool
member _ _ = False

-- | Is this a subset?
-- @(s1 `isSubsetOf` s2)@ tells whether @s1@ is a subset of @s2.
isSubsetOf :: BitMap -> BitMap -> Bool
isSubsetOf _ _ = False

-- | Is this a proper subset? (i.e. a subset but not equal).
isProperSubsetOf :: BitMap -> BitMap -> Bool
isProperSubsetOf a b = a `isSubsetOf` b && not (b `isSubsetOf` a)

-- | Count the bits set in the range [0,i].
rank :: BitMap -> Int -> Int
rank _ _ = 0

-- | Find the index of the ith set bit.
select :: BitMap -> Int -> Maybe Key
select _ _ = Nothing

-- * Construction

-- | /O(1)./ The empty set.
empty :: BitMap
empty = BitMap mempty

-- | /O(1)./ A set of one element.
singleton :: Key -> BitMap
singleton k = insert k empty

-- | Add a value to the set.
insert :: Key -> BitMap -> BitMap
insert k s@(BitMap _) =
    let (i,b) = splitWord k
        _c = maybe (chunkNew i b) (chunkSet b)
    in s

-- | Delete a value in the set.
-- Returns the original set when the value was not present.
delete :: Key -> BitMap -> BitMap
delete _ s = s

-- * Combine

-- | The union of two sets.
union :: BitMap -> BitMap -> BitMap
union _ _ = empty

-- | The difference between two sets.
difference :: BitMap -> BitMap -> BitMap
difference _ b = b

-- | The intersection of two sets.
intersection :: BitMap -> BitMap -> BitMap
intersection _ b = b

-- * Conversion

-- ** List

elems :: BitMap -> [Key]
elems = toAscList

toList :: BitMap -> [Key]
toList _ = []

fromList :: [Key] -> BitMap
fromList _ = empty

-- ** Ordered list

toAscList :: BitMap -> [Key]
toAscList _ = []

toDescList :: BitMap -> [Key]
toDescList _ = []

fromAscList :: [Key] -> BitMap
fromAscList _ = empty

-- * Utility

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

-- | Create a new chunk.
chunkNew :: Word16 -> Word16 -> Chunk
chunkNew i v = LowDensity i 1 (U.singleton v)

-- | Set a bit in a chunk.
chunkSet :: Word16 -> Chunk -> Chunk
chunkSet _v chunk = case chunk of
    LowDensity  i c a -> LowDensity  i c a
    HighDensity i c b -> HighDensity i c b
