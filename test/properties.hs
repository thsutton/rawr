{-# LANGUAGE TemplateHaskell #-}
module Main where

import           Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import           Control.Monad
import           System.Exit
import           System.IO

import Data.Rawr
import qualified Data.Rawr.Bits as Bits

import Generators

prop_reverse :: Property
prop_reverse =
  property $ do
    xs <- forAll $ Gen.list (Range.linear 0 100) Gen.alpha
    (reverse $ reverse xs) === xs

prop_word_splitting :: Property
prop_word_splitting =
  property $ do
    w <- forAll $ Gen.word32 (Range.linear 0 (2^32 - 1))
    Bits.word (Bits.wordIndex w) (Bits.wordValue w) === w

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