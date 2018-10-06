{-# LANGUAGE BangPatterns, MagicHash #-}
module Data.Rawr where

import           Data.Bits
import qualified Data.Vector as V
import           Data.Vector (Vector)

import           Data.Rawr.Bits
import           Data.Rawr.Block (Block)
import qualified Data.Rawr.Block as B

-- | A compressed bitmap.
newtype Bitmap = BM (Vector Block)
  deriving (Eq, Ord)

zeroBits :: Bitmap
zeroBits = BM V.empty

bit :: Word32 -> Bitmap
bit = setBit zeroBits

setBit :: Bitmap -> Word32 -> Bitmap
setBit bm w = bm

clearBit :: Bitmap -> Word32 -> Bitmap
clearBit bm w = bm

complementBit :: Bitmap -> Word32 -> Bitmap
complementBit bm w = bm

testBit :: Bitmap -> Word32 -> Boolean 
testBit bm w =
  let ix = wordIndex w
      v = wordValue w
  in fromMaybe False $ (Block.testBit v <$> findBlock bm ix)

findBlock :: Bitmap -> Index -> Maybe Block
