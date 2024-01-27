{-|
Module      : KingTest
Description : King magic bitboards tests
Copyright   : (c) 2023-2024 Matteo Mariotti
License     : GNU GPL v.3
Stability   : experimental
Portability : POSIX

This module contains the tests for the king attacks implemented as magic bitboards.
-}
{- FOURMOLU_DISABLE -}
{-|
-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use camelCase" #-}
{- FOURMOLU_ENABLE -}
module KingTest (king_tests) where

import Bitboard
import Bits
import Common
import Data.Vector
import Gen
import King
{- FOURMOLU_DISABLE -}
import qualified Test.Falsify.Predicate as P
import qualified Test.Tasty.Falsify as Falsify
{- FOURMOLU_ENABLE -}
import Data.Proxy
import InspectionTest
import Test.MuCheck.TestAdapter.TastyAdapter
import Test.Tasty

king_tests :: TestTree
king_tests = testGroup "King tests" [king_property, king_inspection]

{-# ANN king_property "Test" #-}
king_property :: TestTree
king_property =
    testGroup
        "King property"
        [ Falsify.testProperty "getAttacks" prop_get_attacks,
          Falsify.testProperty "attacks number" prop_attacks_number
        ]

prop_get_attacks :: Falsify.Property ()
prop_get_attacks = do
    s <- Falsify.gen genSquare
    Falsify.assert $
        P.eq
            P..$ ("expected", table ! square2Index s)
            P..$ ("actual", getAttacksProxy (Proxy :: Proxy KingBB) s)

attack_number :: Square -> Int
attack_number A1 = 3
attack_number A8 = 3
attack_number H1 = 3
attack_number H8 = 3
attack_number s
    | s' $&$ m /= 0 = 5
    | otherwise = 8
  where
    m = fileA $|$ fileH $|$ rank1 $|$ rank8
    s' = squareMask s

prop_attacks_number :: Falsify.Property ()
prop_attacks_number = do
    s <- Falsify.gen genSquare
    Falsify.assert $
        P.eq
            P..$ ("expected", attack_number s)
            P..$ ( "actual",
                   case getAttacksProxy (Proxy :: Proxy KingBB) s of
                    AttackBB bb -> popCount bb
                 )
