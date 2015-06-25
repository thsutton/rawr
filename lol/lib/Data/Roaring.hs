{-# LANGUAGE RecordWildCards #-}

module Data.Roaring where

import           Data.Bits
import qualified Data.Vector.Algorithms.Heap as S
import           Data.Vector.Unboxed         (Vector)
import qualified Data.Vector.Unboxed         as V
import qualified Data.Vector.Unboxed.Mutable as M
import           Data.Word

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
    , chunkData  :: Vector Word16
    }
    | ChunkDense
    { chunkIndex :: Word16
    , chunkData  :: Vector Word16
    }

-- | Check to see if a values is in a 'Chunk'.
inChunk :: Word16 -> Chunk -> Bool
inChunk v ChunkSparse{..} = v `V.elem` chunkData
inChunk v ChunkDense{..} =
    let (w, n) = v `quotRem` 16
    in testBit (chunkData V.! fromIntegral w) (fromIntegral n)

-- | Insert a value into a 'Chunk'.
--
--   If a 
insertChunk :: Word16 -> Chunk -> Chunk
insertChunk val c@ChunkDense{..} =
    let (wd', bt') = val `quotRem` 16
        wd = fromIntegral wd'
        bt = fromIntegral bt'
        newData = V.modify (\v -> M.read v wd >>= \o -> M.write v wd (setBit o bt)) chunkData
    in c { chunkData = newData }
insertChunk val c@ChunkSparse{..} =
    if val `V.elem` chunkData
    then c
    else let newData = V.modify S.sort $ val `V.cons` chunkData
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
chunkPop ChunkSparse{..} = V.length chunkData
chunkPop ChunkDense{..} = V.foldl' (\a w -> a + popCount w) 0 chunkData 

-- | Convert a sparse 'Vector' into a dense 'Vector'.
sparseToDense :: Vector Word16 -> Vector Word16
sparseToDense v = v

-- | Convert a dense 'Vector' into a sparse 'Vector'.
denseToSparse :: Vector Word16 -> Vector Word16
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
