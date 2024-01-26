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
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -O -dsuppress-all -dno-suppress-type-signatures -fplugin=Test.Tasty.Inspection.Plugin #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use camelCase" #-}
{- FOURMOLU_ENABLE -}
module KingTest (king_tests) where

import Bitboard
import Bits
import Common
import Data.Vector
import King
import qualified Test.Falsify.Predicate as P
import Test.Tasty
import qualified Test.Tasty.Falsify as Falsify
import Test.Tasty.Inspection
import Gen

goal :: AttackBB
goal = getAttacks @KingBB A1

reference :: AttackBB
reference = table ! square2Index A1

king_tests :: TestTree
king_tests = testGroup "Pawns tests" [king_property, king_inspection]

king_inspection :: TestTree
king_inspection = testGroup "Pawns inspection" [king_specialization, king_inline]

prop_get_attacks :: Falsify.Property ()
prop_get_attacks = do
    s <- Falsify.gen genSquare
    Falsify.assert $
        P.eq
            P..$ ("expected", table ! square2Index s)
            P..$ ("actual", getAttacks @KingBB s)

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
                   case getAttacks @KingBB s of
                    AttackBB bb -> popCount bb
                 )

king_property :: TestTree
king_property =
    testGroup
        "Pawns property"
        [ Falsify.testProperty "getAttacks" prop_get_attacks,
          Falsify.testProperty "attacks number" prop_attacks_number
        ]

king_specialization :: TestTree
king_specialization =
    $( inspectTest
        ((hasNoTypeClasses 'goal) {testName = Just "getAttacks specialization"})
     )

king_inline :: TestTree
king_inline =
    $( inspectTest
        (('goal ==- 'reference) {testName = Just "getAttacks inlining"})
     )
