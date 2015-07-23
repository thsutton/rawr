{-# LANGUAGE RecordWildCards #-}

-- |
-- Module: Data.BitMap
-- Description: Compressed bitmap data structure with good performance.
-- Copyright: (c) Thomas Sutton 2015
-- License: BSD3
-- Maintainer: me@thomas-sutton.id.au
-- Stability: experimental
--
-- A compressed bitmaps with good space and time performance.
--
-- This module is intended to be imported qualified, to avoid name clashes with
-- Prelude functions, e.g.
--
-- >  import           Data.BitMap (BitMap)
-- >  import qualified Data.BitMap as BM
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
module Data.BitMap (
    BitMap,
    null,
    size,
    member,
    empty,
    singleton,
    insert,
    delete,
    union,
    unions,
    difference,
    intersection,
    filter,
    elems,
    toList,
    fromList,
    toAscList,
    toDescList,

    splitWord,
    combineWord,
) where

import           Control.Monad
import           Data.Bits
import           Data.Function
import           Data.Monoid
import qualified Data.Vector                 as V
import qualified Data.Vector.Generic         as G
import qualified Data.Vector.Unboxed         as U
import qualified Data.Vector.Unboxed.Mutable as MU
import           Data.Word
import           Prelude                     (Bool (..), Int, error, flip,
                                              foldl, otherwise, quotRem, (+),
                                              (<), (==), zip, snd)
import qualified Prelude                     as P

-- * Chunks

-- $ Here we define the types and operations on 'Chunk', which stores the low
-- order bits for a section of the key space.

-- | A 'Chunk' represents a contiguous piece of the bitmap keyspace.
--
-- Each chunk covers a 16-bit space but we represent high-density and
-- low-density chunks differently. The 'ChunkHigh' constructor represents high
-- density chunks as 2^16 bits packed together into a vector of words and the
-- 'ChunkLow' constructor represents low density chunks as sorted vector of the
-- particular words present in the chunk.
data Chunk
    = ChunkHigh { chunkIndex :: Word16, chunkData :: U.Vector Word16 }
    | ChunkLow  { chunkIndex :: Word16, chunkData :: U.Vector Word16 }

-- | Population of a 'Chunk' before it is converted to high-density.
threshold :: Int
threshold = 4095

-- | An empty chunk.
chunkEmpty :: Word16 -> Chunk
chunkEmpty ix = ChunkLow ix mempty

chunkSingleton :: Word16 -> Word16 -> Chunk
chunkSingleton ix d = ChunkLow ix (U.singleton d)

-- | Inspect a chunk and normalise the representation based on the
chunkNormalise :: Chunk -> Chunk
chunkNormalise c@ChunkLow{chunkData=v}
    | threshold < chunkSize c = c{chunkData = chunkExpand v}
chunkNormalise c@ChunkHigh{chunkData=v}
    | chunkSize c < threshold = c{chunkData = chunkCollapse v}
chunkNormalise c = c

-- | Check the size of a 'Chunk'.
chunkSize :: Chunk -> Int
chunkSize ChunkLow{chunkData=v} = U.length v
chunkSize ChunkHigh{chunkData=v} = vPop v

-- | Convert a low density vector of words into a high density bitvector.
chunkExpand :: U.Vector Word16 -> U.Vector Word16
chunkExpand = U.foldl set (U.replicate 4096 0)
    where
      -- | Set the appropriate bit in the bit vector. The high 12 bits are the
      -- offset into the vector, the low 4 are the bit in word.
      set :: U.Vector Word16 -> Word16 -> U.Vector Word16
      set u w =
          let (ix',b') = w `quotRem` 16
              ix = convert ix'
              b = convert b'
          in U.modify (\v -> MU.read v ix >>= \o -> MU.write v ix (setBit o b)) u

-- | Convert a high density bitvector into a low density vector of words.
chunkCollapse :: U.Vector Word16 -> U.Vector Word16
chunkCollapse u = snd $ U.ifoldl set (0, U.replicate (convert $ vPop u) 0) u
  where
    toV :: Int -> Int -> Word16
    toV ix bt = (rotate (convert ix) 4) .|. convert (bt + 1)
    set :: (Int, U.Vector Word16) -> Int -> Word16 -> (Int, U.Vector Word16)
    set  acc  ix 0 = acc
    set (n,u) ix w =
        let n' = n + popCount w
            ws = zip [n..n'] [ convert x | x <- [0..15], testBit w x]
            u' = U.modify (\v -> mapM_ (\(n,d)-> MU.write v n d) ws) u
        in (n',u')

vPop :: (G.Vector v b, Bits b) => v b -> Int
vPop = G.foldl (\a v -> a + popCount v) 0

-- * Bitmaps

-- $ Bitmaps are built of 'Chunk's within an index structure which stores the
-- high-order bits.

-- | A Roaring Bitmap structure.
data BitMap = BitMap (V.Vector Chunk)

-- | Split a 'Word32' into 'Word16' of the high-order and low-order bits
--  ('fst' and 'snd' respectively)
splitWord :: Word32 -> (Word16, Word16)
splitWord w = let h = convert $ rotate (0xffff0000 .&. w) 16
                  l = convert $ 0x0000ffff .&. w
              in (h, l)

-- | Combine 'Word16's of the high-order and low-order bits into a 'Word32'.
combineWord :: Word16 -> Word16 -> Word32
combineWord h l = rotate (convert h) (-16) .|. convert l

-- | Convert from one integral type to another.
convert :: (P.Integral a, P.Integral b) => a -> b
convert = P.fromIntegral

-- * Query

null :: BitMap -> Bool
null (BitMap v) = V.null v

size :: BitMap -> Int
size (BitMap v) = V.foldl (\a c -> a + chunkSize c) 0 v

member :: Word32 -> BitMap -> Bool
member e m = error "member is not implemented"

-- * Construction

empty :: BitMap
empty = BitMap (mempty)

singleton :: Word32 -> BitMap
singleton w =
    let (ix, d) = splitWord w
    in BitMap (V.singleton (chunkSingleton ix d))

insert :: Word32 -> BitMap -> BitMap
insert w (BitMap v) =
    let (ix, d) = splitWord w
        v' = v
    in BitMap v'

delete :: Word32 -> BitMap -> BitMap
delete w (BitMap v) =
    let (ix, d) = splitWord w
        v' = v
    in BitMap v'

-- * Combine

union :: BitMap -> BitMap -> BitMap
union a b = error "union is undefined"

unions :: [BitMap] -> BitMap
unions = foldl union empty

difference :: BitMap -> BitMap -> BitMap
difference a b = error "difference is undefined"

intersection :: BitMap -> BitMap -> BitMap
intersection a b = error "intersection is undefined"

-- * Filter

filter :: (Word32 -> Bool) -> BitMap -> BitMap
filter p m = error "filter is undefined"

-- * Conversions

elems :: BitMap -> [Word32]
elems = toList

toList :: BitMap -> [Word32]
toList = toAscList

fromList :: [Word32] -> BitMap
fromList = foldl (flip insert) empty

toAscList :: BitMap -> [Word32]
toAscList (BitMap v) = (V.foldl toDList (id) v) []
  where
    toDList :: ([Word32] -> [Word32]) -> Chunk -> [Word32] -> [Word32]
    toDList f ChunkLow{..} = (\l -> l) . f
    toDList f ChunkHigh{..} = (\l -> l) . f

toDescList :: BitMap -> [Word32]
toDescList m = error "toDescList is undefined"
