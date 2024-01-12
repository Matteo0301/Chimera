{-|
Module      : BitboardTest
Description : Bitboard tests
Copyright   : (c) 2023-2024 Matteo Mariotti
License     : GNU GPL v.3
Stability   : experimental
Portability : POSIX

This module contains the tests for the bitboard module.
-}
{- FOURMOLU_DISABLE -}
{-|
-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use camelCase" #-}
module BitboardTest (Bitboard, emptyBoard, population, bb_tests) where

import Bitboard
import Test.Falsify.Predicate qualified as P
import Test.Tasty
import Test.Tasty.Falsify qualified as Falsify
import Test.Tasty.HUnit
import Test.Tasty.Runners (TestTree (TestGroup))
import Gen

bb_tests :: TestTree
bb_tests = TestGroup "Bitboard tests" [set_get_bit, population_tests]

prop_get_set :: Falsify.Property ()
prop_get_set = do
    bb <- Falsify.gen genBitboard
    x <- Falsify.gen genSquare
    Falsify.assert $
        P.eq P..$ ("expected", True) P..$ ("actual", getSquare (setSquare bb x) x)

prop_unset_set :: Falsify.Property ()
prop_unset_set = do
    x <- Falsify.gen genSquare
    Falsify.assert $
        P.eq
            P..$ ("expected", emptyBoard)
            P..$ ("actual", unsetSquare (setSquare emptyBoard x) x)

prop_unset_empty :: Falsify.Property ()
prop_unset_empty = do
    x <- Falsify.gen genSquare
    Falsify.assert $
        P.eq
            P..$ ("expected", emptyBoard)
            P..$ ("actual", unsetSquare emptyBoard x)

prop_population_set :: Falsify.Property ()
prop_population_set = do
    x <- Falsify.gen genSquare
    Falsify.assert $
        P.eq
            P..$ ("expected", 1)
            P..$ ("actual", population (setSquare emptyBoard x))

prop_population :: Falsify.Property ()
prop_population = do
    bb <- Falsify.gen genBitboard
    Falsify.assert $
        P.ge
            P..$ ("expected", 32)
            P..$ ("actual", population bb)

set_get_bit :: TestTree
set_get_bit =
    testGroup
        "Set/unset squares"
        [ Falsify.testProperty "get . set" prop_get_set,
          Falsify.testProperty "unset . set" prop_unset_set,
          Falsify.testProperty "unset empty" prop_unset_empty,
          Falsify.testProperty "population . set" prop_population_set
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
          Falsify.testProperty "population<=32" prop_population
        ]
