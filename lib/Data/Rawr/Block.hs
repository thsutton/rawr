module Data.Rawr.Block where

import           Data.Bits
import           Data.Vector (Vector)
import qualified Data.Vector.Generic as V
import           Data.Word

import           Data.Rawr.Bits
import           Data.Rawr.Dense (DenseVector)
import qualified Data.Rawr.Dense as D
import           Data.Rawr.Sparse (SparseVector)
import qualified Data.Rawr.Sparse as S

data Block
  = Dense Index DenseVector
  | Sparse Index SparseVector
  deriving (Eq)

threshold :: Int
threshold = 2^16

-- | The high-order bits for the words in a particular 'Block'.
index :: Block -> Index
index (Dense ix _) = ix
index (Sparse ix _) = ix

testBit :: Block -> Value -> Bool
testBit (Dense _ d) v = D.testBit d v
testBit (Sparse _ d) v = error "testBit unimplemented on sparse blocks"

setBit :: Block -> Value -> Block
setBit (Dense i d) v = Dense i (D.setBit d v)
setBit (Sparse i d) v =
  let d' = S.setBit d v
  in if (S.popCount d' >= threshold) then Dense i (D.pack $ S.values d') else Sparse i d'
