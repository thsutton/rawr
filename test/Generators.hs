module Generators where

import           Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import qualified Data.Set as Set
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U

import           Data.Rawr
import           Data.Rawr.Bits
import           Data.Rawr.Block hiding (index)
import qualified Data.Rawr.Dense as Dense
import qualified Data.Rawr.Sparse as Sparse

index :: Gen Index
index = I <$> Gen.word16 (Range.linear 0 (2^16))

value :: Gen Value
value = V <$> Gen.word16 (Range.linear 0 (2^16))

sparsevector :: Gen Sparse.SparseVector
sparsevector = 
  (Sparse.SV . U.fromList . Set.toAscList) <$> Gen.set (Range.linear 0 4096) value

sparse :: Gen Block
sparse =
  Sparse <$> index
         <*> sparsevector

dense :: Gen Block
dense =
  Dense <$> index
        <*> (Set.foldl Dense.setBit Dense.empty <$> Gen.set (Range.linear 4097 (2^16)) value)

block :: Gen Block
block = Gen.choice [sparse, dense]

bitmap :: Gen Bitmap
bitmap =
  return $ BM V.empty