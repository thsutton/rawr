module Data.Roaring.Chunk where

import Control.Applicative
import Data.Bits
import Data.Monoid
import qualified Data.Vector.Unboxed as U
import Data.Word

import Data.Roaring.Utility

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
  deriving (Eq,Show)

-- | 'Chunk's are ordered by their index.
instance Ord Chunk where
    compare c1 c2 = compare (chunkIndex c1) (chunkIndex c2)

-- | Create a new chunk.
chunkNew :: Word16 -> Word16 -> Chunk
chunkNew i v = LowDensity i 1 (U.singleton v)

-- | Extract the 'Word32's stored in a 'Chunk'.
chunkToBits :: Chunk -> [Word32]
chunkToBits (LowDensity  i _ a) = combineWord i <$> U.toList a
chunkToBits (HighDensity i _ a) = U.toList . U.concatMap f $ U.indexed a
  where
    f :: (Int, Word64) -> U.Vector Word32
    f (_p,_bs) = U.map (combineWord i) U.empty


-- | Get a bit from a 'Chunk'.
chunkGet :: Word16 -> Chunk -> Bool
chunkGet v chunk = case chunk of
    LowDensity  _ _ a -> U.elem v a
    HighDensity{} -> False -- TODO(thsutton) implement

-- | Set a bit in a chunk.
--
-- TODO(thsutton) Promote LowDensity chunk when it rises above threshold.
chunkSet :: Word16 -> Chunk -> Chunk
chunkSet v chunk = case chunk of
    LowDensity  i c a -> LowDensity  i c (setL v a)
    HighDensity i c a -> HighDensity i c (setH v a)
  where
    setL :: Word16 -> U.Vector Word16 -> U.Vector Word16
    setL i a = uvInsert a i
    setH :: Word16 -> U.Vector Word64 -> U.Vector Word64
    setH _ a = a -- TODO(thsutton) implement

-- | Clear a bit in a 'Chunk'.
--
-- TODO(thsutton) Demote HighDensity chunk when it falls below threshold.
chunkClear :: Word16 -> Chunk -> Chunk
chunkClear v chunk = case chunk of
    LowDensity i _ a ->
        let a' = clearL v a
            c' = U.length a'
        in LowDensity  i c' a'
    HighDensity i _ a ->
        let a' = clearH v a
            c' = U.sum $ U.map popCount a'
        in HighDensity i c' a'
  where
    clearL :: Word16 -> U.Vector Word16 -> U.Vector Word16
    clearL i a = vDelete a (fromIntegral i)
    clearH :: Word16 -> U.Vector Word64 -> U.Vector Word64
    clearH _ a = a -- TODO(thsutton) implement

-- | Take the union of two 'Chunk's, raising an 'error' if they do not share an
-- index.
mergeChunks :: Chunk -> Chunk -> Chunk
mergeChunks c1 c2 =
    if chunkIndex c1 == chunkIndex c2
        then merge c1 c2
        else error "Attempting to merge incompatible chunks!"
  where
    aPop :: U.Vector Word64 -> Int
    aPop = U.sum . U.map popCount
    aSet :: Word16 -> U.Vector Word64 -> U.Vector Word64
    aSet _i v = v
    packA :: U.Vector Word16 -> U.Vector Word64
    packA _ = mempty
    merge (HighDensity i _ a1) (HighDensity _ _ a2) =
        let a' = U.zipWith (.|.) a1 a2 in HighDensity i (aPop a') a'
    merge (HighDensity i _ ah) (LowDensity  _ _ al) =
        let a' = U.foldr' aSet ah al in HighDensity i (aPop a') a'
    merge (LowDensity  i _ al) (HighDensity _ _ ah) =
        let a' = U.foldr' aSet ah al in HighDensity i (aPop a') a'
    merge (LowDensity  i _ a1) (LowDensity  _ _ a2) =
        let a' = vMerge a1 a2
            n' = U.length a'
        -- TODO(thsutton): Is this eager enough?
        in if n' <= 4096
            then LowDensity i n' a'
            else HighDensity i n' (packA a')
