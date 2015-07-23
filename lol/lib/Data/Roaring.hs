{-# LANGUAGE RecordWildCards #-}

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

import Data.Bits
import Data.Function
import Data.Monoid
import Data.Word
import qualified Data.Vector                 as V
import qualified Data.Vector.Mutable         as VM
import qualified Data.Vector.Algorithms.Heap as S
import qualified Data.Vector.Unboxed         as U
import qualified Data.Vector.Unboxed.Mutable as UM

-- * Bitmaps

newtype BitMap = BitMap (V.Vector Chunk)

instance Monoid BitMap where
    mempty = BitMap mempty
    mappend = union

-- | Calculate the union of two 'BitMap's.
union :: BitMap -> BitMap -> BitMap
union bm1@(BitMap v1) bm2@(BitMap v2)
    | V.null v1 = bm2
    | V.null v2 = bm2
    | otherwise = error "union: Unimplemented"

-- * Chunks

-- | A set of 'Word32' values with a common 'Word16' prefix.
--
--   A sparse chunk contains relatively few elements which are stored in-order
--   in a 'Vector'.
--
--   A dense chunk contains relatively many elements which are stored as a bit
--   string, also represented by a 'Vector'.
data Chunk
    = ChunkSparse
    { chunkIndex :: Word16
    , chunkData  :: U.Vector Word16
    }
    | ChunkDense
    { chunkIndex :: Word16
    , chunkData  :: U.Vector Word16
    }

instance Ord Chunk where
    compare c1 c2 = compare `on` chunkIndex

-- | Create a new, empty, 'Chunk'.
newChunk :: Word16 -> Chunk
newChunk ix = ChunkSparse ix mempty

-- | Check to see if a values is in a 'Chunk'.
inChunk :: Word16 -> Chunk -> Bool
inChunk v ChunkSparse{..} = v `U.elem` chunkData
inChunk v ChunkDense{..} =
    let (w, n) = v `quotRem` 16
    in testBit (chunkData U.! fromIntegral w) (fromIntegral n)

-- | Insert a value into a 'Chunk'.
insertChunk :: Word16 -> Chunk -> Chunk
insertChunk val c@ChunkDense{..} =
    let (wd', bt') = val `quotRem` 16
        wd = fromIntegral wd'
        bt = fromIntegral bt'
        newData = U.modify (\v -> UM.read v wd >>= \o -> UM.write v wd (setBit o bt)) chunkData
    in c { chunkData = newData }
insertChunk val c@ChunkSparse{..} =
    -- TODO(thsutton): Use a binary search for membership testing and
    -- insertion.
    if val `U.elem` chunkData
    then c
    else let newData = U.modify S.sort $ val `U.cons` chunkData
         in normaliseChunk $ c { chunkData = newData }

normaliseChunk :: Chunk -> Chunk
normaliseChunk c = case c of
    ChunkSparse{..}
        | chunkPop c > chunkThreshold -> ChunkDense chunkIndex (sparseToDense chunkData)
        | otherwise -> c
    ChunkDense{..}
        | chunkPop c < chunkThreshold -> ChunkSparse chunkIndex (denseToSparse chunkData)
        | otherwise -> c

-- | Determine the population of a 'Chunk'.
chunkPop :: Chunk -> Int
chunkPop ChunkSparse{..} = U.length chunkData
chunkPop ChunkDense{..} = U.foldl' (\a w -> a + popCount w) 0 chunkData

-- | Convert a sparse 'Vector' into a dense 'Vector'.
sparseToDense :: U.Vector Word16 -> U.Vector Word16
sparseToDense v = v

-- | Convert a dense 'Vector' into a sparse 'Vector'.
denseToSparse :: U.Vector Word16 -> U.Vector Word16
denseToSparse v = v

-- | Threshold above which a 'Chunk' is considered dense.
chunkThreshold :: Int
chunkThreshold = 4095

{-

# Dense Chunks

There are 2^16 bits in a dense chunk, each representing the presence or absence
of a 'Word16' value. These bits are stored in a 'Vector' of 'Word16' values (so
as to line up with sparse chunks).

-}
