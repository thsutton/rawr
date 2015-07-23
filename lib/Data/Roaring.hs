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
import qualified Data.Vector.Generic         as VG
import qualified Data.Vector.Mutable         as VM
import qualified Data.Vector.Algorithms.Heap as S
import qualified Data.Vector.Unboxed         as U
import qualified Data.Vector.Unboxed.Mutable as UM

import Data.Roaring.Utility

-- * Bitmaps

newtype BitMap = BitMap (V.Vector Chunk)
  deriving (Eq, Show)

instance Monoid BitMap where
    mempty = BitMap mempty
    mappend = union

-- | Calculate the union of two 'BitMap's.
union :: BitMap -> BitMap -> BitMap
union bm1@(BitMap v1) bm2@(BitMap v2)
    | V.null v1 = bm2
    | V.null v2 = bm2
    | otherwise = BitMap (vMergeWith chunkMerge v1 v2)

isElem :: Word32 -> BitMap -> Bool
isElem w (BitMap bm) =
    let (h,l) = splitWord w
    in error "isElem: unimplemented"

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
  deriving (Eq, Show)

-- | Chunks are ordered by index.
instance Ord Chunk where
    compare = compare `on` chunkIndex

-- | Create a new, empty, 'Chunk'.
chunkNew :: Word16 -> Chunk
chunkNew ix = ChunkSparse ix mempty

-- | Merge two 'Chunk's with the same index.
--
--   This function will 'error' when called on chunks with different indexes.
chunkMerge :: Chunk -> Chunk -> Chunk
chunkMerge c1 c2
    | chunkIndex c1 /= chunkIndex c2 = error $ "chunkMerge: cannot merge chunks with different indexes: " <> show (chunkIndex c1) <> " and " <> show (chunkIndex c2)
    | otherwise = chunkNormalise $ case (c1, c2) of
        (ChunkDense  ix d1, ChunkDense  _ d2) -> ChunkDense ix (U.zipWith (.|.) d1 d2)
        (ChunkDense  ix  _, ChunkSparse _ s2) -> U.foldl' (flip chunkInsert) c1 s2
        (ChunkSparse ix s1, ChunkSparse _ s2) -> ChunkSparse ix (vMergeWith const s1 s2)
        (ChunkSparse ix s1, ChunkDense  _  _) -> U.foldl' (flip chunkInsert) c2 s1

-- | Check to see if a values is in a 'Chunk'.
chunkElem :: Word16 -> Chunk -> Bool
chunkElem v ChunkSparse{..} = v `U.elem` chunkData
chunkElem v ChunkDense{..} =
    let (w, n) = v `quotRem` 16
    in testBit (chunkData U.! fromIntegral w) (fromIntegral n)

-- | Insert a value into a 'Chunk'.
chunkInsert :: Word16 -> Chunk -> Chunk
chunkInsert val c = case c of
    ChunkDense{..} ->
        let (wd', bt') = val `quotRem` 16
            wd = fromIntegral wd'
            bt = fromIntegral bt'
            newData = U.modify (\v -> UM.read v wd >>= \o -> UM.write v wd (setBit o bt)) chunkData
        in c { chunkData = newData }
    ChunkSparse{..} ->
        -- TODO(thsutton): Use a binary search for membership testing and
        -- insertion.
        if val `U.elem` chunkData
        then c
        else let newData = U.modify S.sort $ val `U.cons` chunkData
             in chunkNormalise $ c { chunkData = newData }

-- | Determine the population of a 'Chunk'.
chunkPop :: Chunk -> Int
chunkPop ChunkSparse{..} = U.length chunkData
chunkPop ChunkDense{..} = U.foldl' (\a w -> a + popCount w) 0 chunkData

-- | Normalise a 'Chunk' based on the number of bits it contains.
chunkNormalise :: Chunk -> Chunk
chunkNormalise c =
    let chunkSize = chunkPop c
    in case c of
        ChunkSparse{..}
            | chunkSize > chunkThreshold -> ChunkDense chunkIndex (sparseToDense chunkData)
            | otherwise -> c
        ChunkDense{..}
            | chunkSize < chunkThreshold -> ChunkSparse chunkIndex (denseToSparse chunkData)
            | otherwise -> c

-- | Convert a sparse 'Vector' into a dense 'Vector'.
sparseToDense :: U.Vector Word16 -> U.Vector Word16
sparseToDense = U.foldl' set (U.replicate 4096 0)
  where
    bit :: Word16 -> Word16
    bit w = bit $ ((fromIntegral $ w .&. 0x000f) - 1)
    set :: U.Vector Word16 -> Word16 -> U.Vector Word16
    set v w = vAlter (\x -> Just $ x .&. bit w) (fromIntegral $ shiftR w 12) v

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


-- * Utility

-- | Merge two 'Vector's.
--
--   Elements are compared with lower elements being added to the result array,
--   equal elements being merged using the supplied argument, and higher
--   elements being retained for the next cycle.
vMergeWith
    :: (VG.Vector v a, Ord a)
    => (a -> a -> a)
    -> v a
    -> v a
    -> v a
vMergeWith f as bs
    | VG.null as = bs
    | VG.null bs = as
    | otherwise =
        let a   = VG.head as
            as' = VG.tail as
            b   = VG.head bs
            bs' = VG.tail bs
        in case a `compare` b of
            LT ->  a        `VG.cons` (vMergeWith f as' bs )
            EQ -> (a `f` b) `VG.cons` (vMergeWith f as' bs')
            GT ->        b  `VG.cons` (vMergeWith f as  bs')
