module Data.Rawr.Sparse where

import           Data.Bits
import           Data.Maybe
import           Data.Vector.Unboxed (Vector)
import qualified Data.Vector.Unboxed as V
import           Data.Word

import Data.Rawr.Bits

-- | A Vector of 'Value's stored in sorted order.
newtype SparseVector = SV (Vector Word16)
  deriving (Eq, Ord)

empty :: SparseVector
empty = SV (V.empty)

-- | Find the position of a value if it is present in the vector.
--
-- TODO: A better search
findBit :: SparseVector -> Value -> Maybe Int
findBit (SV d) (V v) = V.elemIndex v d

testBit :: SparseVector -> Value -> Bool
testBit d v = isJust $ findBit d v

setBit :: SparseVector -> Value -> SparseVector
setBit (SV d) (V v) =
  let d' = d
  in SV d'

popCount :: SparseVector -> Int
popCount (SV v) = V.length v