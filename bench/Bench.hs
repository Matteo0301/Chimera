{-|
Module      : Main
Description : Benchmark suite
Copyright   : (c) 2023-2024 Matteo Mariotti
License     : GNU GPL v.3
Stability   : experimental
Portability : POSIX

This program benchmarks the performance of the various libraries implementing the project.
-}
{- FOURMOLU_DISABLE -}
{-|
-}
module Main where

import BitboardBench
import Test.Tasty.Bench

-- Our benchmark harness.
main :: IO ()
main =
    defaultMain
        [ bitboard_benches
        ]
