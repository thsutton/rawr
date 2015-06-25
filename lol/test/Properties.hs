{-# LANGUAGE RecordWildCards #-}

module Main where

import Test.QuickCheck

import Data.Roaring

prop_inChunk :: Word16 -> Chunk -> Bool
prop_inChunk w c = w `inChunk` insertChunk w c

main :: ()
main = return ()
