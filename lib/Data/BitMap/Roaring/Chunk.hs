module Data.BitMap.Roaring.Chunk where

import           Control.Applicative
import           Control.Monad
import           Control.Monad.Trans.State
import           Data.Bits
import           Data.Function
import           Data.Monoid
import qualified Data.Vector.Algorithms.Heap as S
import qualified Data.Vector.Unboxed         as U
import qualified Data.Vector.Unboxed.Mutable as M
import           Data.Word

import           Data.BitMap.Roaring.Chunk.High (HDVector (..))
import qualified Data.BitMap.Roaring.Chunk.High as H
import           Data.BitMap.Roaring.Chunk.Low  (LDVector (..))
import qualified Data.BitMap.Roaring.Chunk.Low  as L
import           Data.BitMap.Roaring.Utility

-- | Chunks are identified by a 'Word16' index.
type Index = Word16

-- | A chunk representing the keys which share particular 16 high-order bits.
--
-- Chunk with low density (i.e. no more than 4096 members) are represented as a
-- sorted array of their low 16 bits. Chunks with high density (i.e. more than
-- 4096 members) are represented by a bit vector.
data Chunk
    = LowDensity
        { chunkIndex :: Index
        , chunkArray :: LDVector
        }
    | HighDensity
        { chunkIndex :: Index
        , chunkBits  :: HDVector
        }
  deriving (Eq, Show)

-- | 'Chunk's are ordered by their index.
instance Ord Chunk where
    compare = compare `on` chunkIndex

instance Bits Chunk where
  bitSize _ = 2^16
  bitSizeMaybe _ = Just (2^16)
  isSigned _ = False

  (.|.) = union
  (.&.) = intersection

  testBit (LowDensity ix a) i = L.testBit a (fromIntegral i)
  testBit (HighDensity ix a) i = H.testBit a (fromIntegral i)

  bit i = singleton (fromIntegral i)

  popCount (LowDensity ix a) = L.popCount a
  popCount (HighDensity ix a) = H.popCount a

singleton :: Word32 -> Chunk
singleton i =
  let (ix, b) = splitWord i
  in chunkNew ix b

-- | Create a new chunk.
chunkNew :: Index -> Word16 -> Chunk
chunkNew i v = LowDensity i (L.singleton v)

-- | Add a word into a chunk.
set :: Word16 -> Chunk -> Chunk
set b c@(HighDensity i bs)
    | H.testBit bs b = c
    | otherwise      = HighDensity i (H.setBit bs b)
set b c@(LowDensity i bs)
    | L.testBit bs b       = c
    | otherwise            = repackChunk $ LowDensity i (L.setBit bs b)

toList :: Chunk -> [Word32]
toList (LowDensity i bs) = map (combineWord i) $ L.toList bs
toList (HighDensity i bs) = map (combineWord i) $ H.toList bs

bits :: Word64 -> [Word16]
bits w = foldr abit [] [0..63]
  where
    abit :: Int -> [Word16] -> [Word16]
    abit i l = if testBit w i
               then (fromIntegral i) : l
               else l

chunkCheck :: Word16 -> Chunk -> Bool
chunkCheck w (LowDensity _ bs) = L.testBit bs w
chunkCheck w (HighDensity _ bs) = H.testBit bs w

chunkSet :: Word16 -> Chunk -> Chunk
chunkSet w c@(HighDensity i bs)
    | H.testBit bs w = c
    | otherwise      = HighDensity i (H.setBit bs w)
chunkSet w c@(LowDensity i bs)
    | L.testBit bs w = c
    | otherwise    = LowDensity i (L.setBit bs w)

chunkClear :: Word16 -> Chunk -> Chunk
chunkClear w c
    | chunkCheck w c =
        case c of
          LowDensity i bs  -> LowDensity i (L.clearBit bs w)
          HighDensity i bs -> HighDensity i (H.clearBit bs w)
    | otherwise = c

-- | Take the union of two 'Chunk's.
--
-- Postcondition: popCount (a `union` b) >= (popCount a) + (popCount b)
union :: Chunk -> Chunk -> Chunk
union a b
  | chunkIndex a == chunkIndex b = work a b
  | otherwise = error "Cannot take union of chunks with different indexes!"
  where
    work (HighDensity i as) (HighDensity _ bs) =
      HighDensity i (as `H.union` bs)
    work (HighDensity i as) (LowDensity _ bs) =
      HighDensity i (as `H.union` toHDVector bs)
    work (LowDensity i as) (HighDensity _ bs) =
      HighDensity i (toHDVector as `H.union` bs)
    work (LowDensity i as) (LowDensity _ bs) =
      repackChunk $ LowDensity i (as `L.union` bs)

-- | Take the intersection of two 'Chunk's.
--
-- TODO: Maintain the density invariant.
intersection a b
  | chunkIndex a == chunkIndex b = work a b
  | otherwise = error "Cannot take intersection of chunks with different indexes!"
  where
    work (LowDensity ia a) (LowDensity ib b) =
      LowDensity ia (a `L.intersection` b)
    work (HighDensity ia a) (LowDensity ib b) =
      LowDensity ia (toLDVector a `L.intersection` b)
    work (LowDensity ia a) (HighDensity ib b) =
      LowDensity ia (a `L.intersection` toLDVector b)
    work (HighDensity ia a) (HighDensity ib b) =
      repackChunk $ HighDensity ia (a `H.intersection` b)

xor :: Chunk -> Chunk -> Chunk
xor a b
    | chunkIndex a == chunkIndex b = work a b
    | otherwise = error "Cannot take xor of chunks with different indexes!"
  where
    work :: Chunk -> Chunk -> Chunk
    work (LowDensity ia as) (LowDensity ib bs) =
      repackChunk $ LowDensity ia (as `L.xor` bs)
    work (LowDensity ia as) (HighDensity ib bs) =
      repackChunk $ HighDensity ia (toHDVector as `H.xor` bs)
    work (HighDensity ia as) (LowDensity ib bs) =
      repackChunk $ HighDensity ia (as `H.xor` toHDVector bs)
    work (HighDensity ia as) (HighDensity ib bs) =
      repackChunk $ HighDensity ia (as `H.xor` bs)

-- * Queries

null :: Chunk -> Bool
null c = popCount c == 0

-- * Utility

-- | Repack a 'Chunk' to enforce the density invariant.
repackChunk :: Chunk -> Chunk
repackChunk c@(LowDensity ix v)
    | L.popCount v >= 4096 = HighDensity ix (toHDVector v)
    | otherwise = c
repackChunk c@(HighDensity ix v)
    | H.popCount v < 4096 = LowDensity ix (toLDVector v)
    | otherwise = c

-- | Pack a low-density vector into a high-density vector.
toHDVector :: LDVector -> HDVector
toHDVector (LDVector bs) = U.foldl' (\v b -> H.setBit v b) H.empty bs

-- | Unpack a high-density vector to a low-density vector.
toLDVector :: HDVector -> LDVector
toLDVector v@(HDVector ws) =
    let n = H.popCount v
        bs = U.generate n (\i -> fromIntegral $ select i v)
    in LDVector bs
  where
    -- | Select the nth set bit.
    select :: Int -> HDVector -> Int
    select i (HDVector v) =
      let runningCount = evalState (U.mapM (\c -> modify (+ popCount c) >> get) v) 0
          (p,r) = U.span (\a -> a < i) runningCount
      in -1
