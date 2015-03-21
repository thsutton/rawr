{-# LANGUAGE TemplateHaskell #-}

module Main where

import Control.Monad
import qualified Data.BitMap.Roaring as R
import Data.Word
import System.Exit

import Test.QuickCheck

-- | Check that "Data.IntSet" and "Data.BitMap.Roaring" agree about a set when
-- building from the same list of inputs.
prop_intset_roaring_agree :: NonEmptyList Word32 -> Bool
prop_intset_roaring_agree (NonEmpty _) = False

-- | The empty set is null.
prop_null_empty :: Bool
prop_null_empty = R.null R.empty

-- | Singleton sets are not null.
prop_null_singleton :: Word32 -> Bool
prop_null_singleton i = not (R.null $ R.insert i R.empty)

-- | Larger sets are not null.
prop_null_fromList :: NonEmptyList Word32 -> Bool
prop_null_fromList (NonEmpty is) = not . R.null $ R.fromList is

-- | Empty sets have size zero.
prop_size_empty :: Bool
prop_size_empty = 0 == R.size R.empty

-- | Singletons have size 1, then size 0 when deleted.
prop_size_delete_singleton :: Word32 -> Bool
prop_size_delete_singleton i =
    let s = R.singleton i
        s' = R.delete i s
    in R.size s == 1 && R.size s' == 0

--
-- Use Template Haskell to automatically run all of the properties above.
--

return []
runTests :: IO Bool
runTests = $quickCheckAll

main :: IO ()
main = do
    result <- runTests
    unless result exitFailure
