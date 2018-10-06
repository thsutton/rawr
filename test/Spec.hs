{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Main where

import           Control.Monad
import           System.Exit
import           Test.QuickCheck
import           Test.QuickCheck.Instances  ()

import qualified Data.Vector.Unboxed as U
import Data.Word
import Data.List (nub, sort)

import Data.Rawr

instance Arbitrary Block where
  arbitrary = do
    idx <- arbitrary
    wds <- arbitrary
    return (Block $ idx `U.cons` (U.fromList . nub . sort $ wds))

prop_insert_length b w =
  let b' = insert b w
      delta = count b' - count b
  in delta == 0 || delta == 1

prop_remove_length b w =
  let b' = remove b w
      delta = count b - count b'
  in delta == 0 || delta == 1

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
