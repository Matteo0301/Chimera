{-|
Module      : BitboardBench
Description : Bitboard benchmarks
Copyright   : (c) 2023-2024 Matteo Mariotti
License     : GNU GPL v.3
Stability   : experimental
Portability : POSIX

This module benchmarks the performance of the Bitboard library.
-}
{- FOURMOLU_DISABLE -}
{-|
-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use camelCase" #-}
{-# HLINT ignore "Redundant bracket" #-}
module BitboardBench (bitboard_benches) where

import Bitboard
import Test.Tasty.Bench

bitboard_benches :: Benchmark
bitboard_benches =
    bgroup
        "Bitboard"
        [ bench "population empty" $ whnf population emptyBoard,
          bench "population initial" $ whnf (population) initialBoard,
          bench "getSquare empty" $ whnf ((getSquare emptyBoard)) A1,
          bench "getSquare full" $ whnf ((getSquare initialBoard)) A1,
          bench "setSquare empty" $ whnf ((setSquare emptyBoard)) A1,
          bench "unsetSquare empty" $ whnf ((unsetSquare emptyBoard)) A1,
          bench "unsetSquare full" $ whnf ((unsetSquare initialBoard)) A1,
          bench "setSquare empty 32 times" $
            whnf (foldl' (<<>>) emptyBoard) ([A1 .. H2] ++ [A7 .. H8])
        ]
