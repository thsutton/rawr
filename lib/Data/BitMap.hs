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

import           Control.Applicative ((<$>))
import           Control.Monad
import           Data.Bits
import           Data.Function
import           Data.Monoid
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV
import qualified Data.Vector.Generic as G
import qualified Data.Vector.Algorithms.Insertion as A
import qualified Data.Vector.Algorithms.Search as A
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as MU
import           Data.Word
import           Prelude                     (Bool(..), Eq(..), Int, Maybe(..),
                                              Show(..),
                                              compare, error, flip, foldl,
                                              otherwise, quotRem, zip, snd,
                                              (+), (++), (-), (<), (==), (/=))
import qualified Prelude as P

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
  deriving (Show, Eq)

-- | Population of a 'Chunk' before it is converted to high-density.
threshold :: Int
threshold = 4095

-- | An empty chunk.
chunkEmpty :: Word16 -> Chunk
chunkEmpty ix = ChunkLow ix mempty

-- | A chunk containing a single value.
chunkSingleton :: Word16 -> Word16 -> Chunk
chunkSingleton ix d = ChunkLow ix (U.singleton d)

-- | Check the size of a 'Chunk'.
chunkSize :: Chunk -> Int
chunkSize ChunkLow{chunkData=v} = U.length v
chunkSize ChunkHigh{chunkData=v} = vPop v

-- | Check whether a value is included in a chunk.
chunkElem :: Word16 -> Chunk -> Bool
chunkElem w ChunkLow{chunkData=v} = U.elem w v
chunkElem w ChunkHigh{chunkData=v} =
    let (wd', bt') = w `quotRem` 16
        wd = convert wd
        bt = convert bt'
    in testBit (v U.! wd) bt

-- | Insert a value into a chunk.
--
-- @chunkInsert w (chunkInsert w c) == chunkInsert w c@
--
-- @TODO(thsutton) Optimise correctly.
chunkInsert :: Word16 -> Chunk -> Chunk
chunkInsert w c
    | chunkElem w c = c
    | otherwise = case c of
        ChunkLow{chunkData=v} -> c{chunkData = U.modify A.sort (U.cons w v)}
        ChunkHigh{chunkData=v} -> c

-- | Remove a value from a chunk.
--
-- @TODO(thsutton) Optimise correctly.
chunkDelete :: Word16 -> Chunk -> Chunk
chunkDelete w c
    | chunkElem w c = case c of
        ChunkLow{chunkData=v} -> c{chunkData = U.filter (/= w) v}
        ChunkHigh{chunkData=v} -> c
    | otherwise     = c

-- | Inspect a chunk and normalise the representation based on the
chunkNormalise :: Chunk -> Chunk
chunkNormalise c@ChunkLow{chunkData=v}
    | threshold < chunkSize c = c{chunkData = chunkExpand v}
chunkNormalise c@ChunkHigh{chunkData=v}
    | chunkSize c < threshold = c{chunkData = chunkCollapse v}
chunkNormalise c = c

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
  deriving (Show, Eq)

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

-- | Find a chunk in a bitmap with an index.
--
-- @TODO(thsutton) Use a binary search here.
findChunk :: Word16 -> BitMap -> Maybe Int
findChunk ix (BitMap v) = V.findIndex (\c -> ix == chunkIndex c) v

-- | Insert a new chunk into a bitmap.
--
-- Precondition: no chunk with the index exists in the bitmap.
-- Postcondition: new chunk is present in bitmap, in sorted order.
insertChunk :: Chunk -> BitMap -> BitMap
insertChunk c (BitMap v) =
    BitMap (V.modify (A.sortBy (compare `on` chunkIndex)) (V.cons c v))

-- | Modify a chunk in a bitmap.
modifyChunk :: Word16 -> (Maybe Chunk -> Maybe Chunk) -> BitMap -> BitMap
modifyChunk ix f m@(BitMap v) =
    let mi = findChunk ix m
        mc = f ((v V.!) <$> mi)
    in case (mi,mc) of
         -- Do nothing
         (Nothing, Nothing) -> m
         -- Insert new chunk
         (Nothing, Just c) -> insertChunk c m
         -- Delete existing chunk
         -- @TODO(thsutton) Move should be more efficient.
         (Just i, Nothing) ->
             let l = V.length v
                 l' = l - 1
                 ls = l - i - 1
             in BitMap . V.take (l - 1) $ V.modify (\w -> do
                 let t = MV.slice i ls w
                 let s = MV.slice (i+1) ls w
                 MV.move s t
                 ) v
         -- Replace existing chunk
         (Just i, Just c) -> BitMap $ V.modify (\v -> MV.write v i c) v

-- * Query

null :: BitMap -> Bool
null (BitMap v) = V.null v

size :: BitMap -> Int
size (BitMap v) = V.foldl (\a c -> a + chunkSize c) 0 v

member :: Word32 -> BitMap -> Bool
member e m@(BitMap v) =
    let (ix, w) = splitWord e
    in case findChunk ix m of
       Nothing -> False
       Just i -> chunkElem w (v V.! i)

-- * Construction

empty :: BitMap
empty = BitMap (mempty)

singleton :: Word32 -> BitMap
singleton w =
    let (ix, d) = splitWord w
    in BitMap (V.singleton (chunkSingleton ix d))

insert :: Word32 -> BitMap -> BitMap
insert w m@(BitMap v) =
    let (ix, d) = splitWord w
        f Nothing  = Just (chunkSingleton ix d)
        f (Just c) = Just (chunkInsert d c)
    in modifyChunk ix f m

delete :: Word32 -> BitMap -> BitMap
delete w m =
    let (ix, d) = splitWord w
    in modifyChunk ix (f d) m
  where
    f _ Nothing = Nothing
    f d (Just c) =
        let c' = chunkDelete d c
        in if chunkSize c' == 0
           then Nothing
           else Just (chunkNormalise c')

-- * Combine

-- | Merge two bitmaps.
--
-- @TODO(thsutton) Implement correctly.
union :: BitMap -> BitMap -> BitMap
union a b
    | null a = b
    | null b = a
    | size a < size b = foldl (flip insert) b $ toList a
    | otherwise = foldl (flip insert) a $ toList b

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

toDescList :: BitMap -> [Word32]
toDescList (BitMap v) = (V.foldl toDList id v) []
  where
    toDList :: ([Word32] -> [Word32]) -> Chunk -> [Word32] -> [Word32]
    toDList f ChunkLow{..} = unpackedDList chunkIndex chunkData . f
    toDList f ChunkHigh{..} = packedDList chunkIndex chunkData . f

    packedDList :: Word16 -> U.Vector Word16 -> ([Word32] -> [Word32])
    packedDList ix v = U.ifoldl (word ix) id v
    word ix acc i w =
        (abit 0) . (abit 1) . (abit 2) . (abit 3) . (abit 4) . (abit 5) .
        (abit 6) . (abit 7) . (abit 8) . (abit 9) . (abit 10) . (abit 11) .
        (abit 12) . (abit 13) . (abit 14) . (abit 15) . acc
      where
        c n = combineWord ix (rotate (convert i) 4 .|. convert n)
        abit n l = if testBit w n then c n:l else l

    -- Unpack a sparse vector into a dlist.
    unpackedDList :: Word16 -> U.Vector Word16 -> ([Word32] -> [Word32])
    unpackedDList ix v = U.foldl (\acc w-> (combineWord ix w:) . acc) id v

toAscList :: BitMap -> [Word32]
toAscList = P.reverse . toDescList
