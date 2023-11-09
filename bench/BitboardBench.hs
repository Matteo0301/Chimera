{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use camelCase" #-}
module BitboardBench (bitboard_benches) where

import Bitboard
import Prelude.Linear (forget)
import Test.Tasty.Bench

bitboard_benches :: Benchmark
bitboard_benches =
    bgroup
        "Bitboard"
        [ bench "population empty" $ whnf (forget getPopulation) emptyBoard,
          bench "population initial" $ whnf (forget getPopulation) initialBoard,
          bench "getSquare empty" $ whnf (forget (getSquare emptyBoard)) A1,
          bench "getSquare full" $ whnf (forget (getSquare initialBoard)) A1,
          bench "setSquare empty" $ whnf (forget (setSquare emptyBoard)) A1,
          bench "unsetSquare empty" $ whnf (forget (unsetSquare emptyBoard)) A1,
          bench "unsetSquare full" $ whnf (forget (unsetSquare initialBoard)) A1,
          bench "setSquare empty 32 times" $
            whnf (foldl (<<>>) emptyBoard) ([A1 .. H2] ++ [A7 .. H8])
        ]
