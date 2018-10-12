{-# LANGUAGE TemplateHaskell #-}
module Main where

import           Control.Monad
import qualified Data.Set as Set
import           Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import           System.Exit
import           System.IO

import Data.Rawr
import qualified Data.Rawr as Rawr
import qualified Data.Rawr.Bits as Bits

import Generators

prop_word_splitting :: Property
prop_word_splitting =
  property $ do
    w <- forAll $ Gen.word32 (Range.linear 0 (2^32 - 1))
    Bits.word (Bits.wordIndex w) (Bits.wordValue w) === w

prop_intset_bitmap_agree :: Property
prop_intset_bitmap_agree =
  property $ do
    ws <- forAll $ Gen.set (Range.linear 0 (2^16)) (Gen.word32 (Range.linear 0 (2^32 - 1)))
    let r = Rawr.toAscList . Rawr.fromList . Set.toList $ ws
    let s = Set.toAscList $ ws
    r === s

tests :: IO Bool
tests =
  checkParallel $$discover

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  hSetBuffering stderr LineBuffering

  results <- sequence [
      tests
      -- Just the one suite at the moment.
    ]

  unless (and results) exitFailure