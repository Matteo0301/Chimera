{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use camelCase" #-}
{-# HLINT ignore "Redundant bracket" #-}
module BitboardBench (bitboard_benches) where

import Bitboard
import Test.Tasty.Bench
import Prelude

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
            whnf (foldl (<<>>) emptyBoard) ([A1 .. H2] ++ [A7 .. H8])
        ]
