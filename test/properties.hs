{-# LANGUAGE TemplateHaskell #-}

module Main where

import           Control.Monad
import           Data.List
import           Data.Monoid
import qualified Data.Set        as S
import           Data.Word
import           System.Exit
import           Test.QuickCheck

import qualified Data.BitMap.Roaring         as R
import qualified Data.BitMap.Roaring.Utility as R

-- * Check utility functions

-- | id == uncurry combineWord . splitWord
prop_splitWord_combineWord_id :: Word32 -> Bool
prop_splitWord_combineWord_id w =
    w == (uncurry R.combineWord . R.splitWord $ w)

-- | The empty set is null.
prop_null_empty :: Bool
prop_null_empty = R.null R.empty

-- | Singleton sets are not null.
prop_not_null_singleton :: Word32 -> Bool
prop_not_null_singleton i = not . R.null $ R.singleton i

-- | Larger sets are not null.
prop_not_null_fromList :: NonEmptyList Word32 -> Bool
prop_not_null_fromList (NonEmpty is) = not . R.null $ R.fromList is

-- | Empty sets have size zero.
prop_size_empty :: Bool
prop_size_empty = 0 == R.size R.empty

-- | Singletons have size one.
prop_size_singleton :: Word32 -> Bool
prop_size_singleton i = 1 == R.size (R.singleton i)

-- | Singletons have size 1, then size 0 when deleted.
prop_size_delete_singleton :: Word32 -> Bool
prop_size_delete_singleton i =
    let s = R.singleton i
        s' = R.delete i s
    in R.size s == 1 && R.size s' == 0

-- | Singletons are empty when the sole item is deleted.
prop_null_delete_singleton :: Word32 -> Bool
prop_null_delete_singleton i =
    R.null . R.delete i $ R.singleton i

-- | 'toAscList' produces sorted lists.
prop_toAscList_sorted :: NonEmptyList Word32 -> Bool
prop_toAscList_sorted (NonEmpty l) =
    let l' = S.toAscList (S.fromList l)
    in l' == sort l'

-- | 'toDescList' produces sorted lists.
prop_toDescList_sorted :: NonEmptyList Word32 -> Bool
prop_toDescList_sorted (NonEmpty l) =
    let l' = S.toDescList (S.fromList l)
    in l' == sortBy (flip compare) l'

-- | "Data.IntSet" and "Data.BitMap.Roaring" agree about a set when building
-- from the same list of inputs.
prop_intset_roaring_agree :: NonEmptyList Word32 -> Bool
prop_intset_roaring_agree (NonEmpty l) =
    let r = R.toAscList $ R.fromList l
        s = S.toAscList $ S.fromList l
    in r == s

-- | Every item in the source list should be an element.
prop_map_elem_fromList :: NonEmptyList Word32 -> Bool
prop_map_elem_fromList (NonEmpty l) =
    let r = R.fromList l
    in all (`R.member` r) l

-- | union s1 s2 == fromList (toList s1 <> toList s2)
prop_union_fromList :: NonEmptyList Word32 -> NonEmptyList Word32 -> Bool
prop_union_fromList (NonEmpty as) (NonEmpty bs) =
    let q = R.fromList as
        r = R.fromList bs
        qr = R.fromList (as <> bs)
    in (R.toAscList qr == R.toAscList (q `R.union` r))

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
