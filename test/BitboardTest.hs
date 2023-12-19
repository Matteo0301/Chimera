{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use camelCase" #-}
module BitboardTest (Bitboard, emptyBoard, population, bb_tests) where

import Bitboard
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck as QC
import Test.Tasty.Runners (TestTree (TestGroup))
import Types

bb_tests :: TestTree
bb_tests = TestGroup "Bitboard tests" [set_get_bit, population_tests]

set_get_bit :: TestTree
set_get_bit =
    testGroup
        "Set/unset squares"
        [ QC.testProperty "get . set" $
            \x -> getSquare (setSquare emptyBoard x) x,
          QC.testProperty "unset . set" $
            \x -> unsetSquare (setSquare emptyBoard x) x == emptyBoard,
          QC.testProperty "set . unset" $
            \x -> unsetSquare emptyBoard x == emptyBoard,
          QC.testProperty "population . set" $
            \x -> population (setSquare emptyBoard x) == 1
        ]

population_tests :: TestTree
population_tests =
    testGroup
        "Population tests"
        [ testCase "Population: empty" $
            population emptyBoard
                @?= 0,
          testCase "Population: A1" $
            population (emptyBoard <<>> A1)
                @?= 1,
          testCase "Population: D5" $
            population (emptyBoard <<>> D5)
                @?= 1,
          testCase "Population: white pawns" $
            population
                (emptyBoard <<>> A2 <<>> B2 <<>> C2 <<>> D2 <<>> E2 <<>> F2 <<>> G2 <<>> H2)
                @?= 8,
          QC.testProperty "population<=32" $
            \x -> population x <= 32
        ]
