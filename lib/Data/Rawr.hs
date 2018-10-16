{-# LANGUAGE BangPatterns, MagicHash #-}
module Data.Rawr where

import           Data.Bits (Bits)
import           Data.Maybe
import qualified Data.Vector as V
import           Data.Vector (Vector)
import           Data.Version (Version)
import           Data.Word

import           Data.Rawr.Bits
import           Data.Rawr.Block (Block)
import qualified Data.Rawr.Block as B

import qualified Paths_rawr as Paths

version :: Version
version = Paths.version

-- | A compressed bitmap.
newtype Bitmap = BM (Vector Block)
  deriving (Eq)

zeroBits :: Bitmap
zeroBits = BM V.empty

bit :: Word32 -> Bitmap
bit = setBit zeroBits

setBit :: Bitmap -> Word32 -> Bitmap
setBit bm w =
  let ix = wordIndex w
      v  = wordValue w
  in withBlock bm ix (flip B.setBit v)

clearBit :: Bitmap -> Word32 -> Bitmap
clearBit bm w = bm

complementBit :: Bitmap -> Word32 -> Bitmap
complementBit bm w = bm

testBit :: Bitmap -> Word32 -> Bool
testBit bm w =
  let ix = wordIndex w
      v = wordValue w
  in fromMaybe False $ (flip B.testBit v <$> findBlock bm ix)

findBlock :: Bitmap -> Index -> Maybe Block
findBlock (BM blocks) ix = Nothing

withBlock :: Bitmap -> Index -> (Block -> Block) -> Bitmap
withBlock bm ix f =
  case findBlock bm ix of
    Nothing -> insertBlock bm (f $ B.empty ix)
    Just b  -> replaceBlock bm (f b)

insertBlock :: Bitmap -> Block -> Bitmap
insertBlock bm b = bm

replaceBlock :: Bitmap -> Block -> Bitmap
replaceBlock bm b = bm

fromList :: [Word32] -> Bitmap
fromList = foldl (setBit) zeroBits

toAscList :: Bitmap -> [Word32]
toAscList (BM cs) = concatMap B.blockToBits $ V.toList cs
