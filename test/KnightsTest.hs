{-|
Module      : KnightsTest
Description : Knights magic bitboards tests
Copyright   : (c) 2023-2024 Matteo Mariotti
License     : GNU GPL v.3
Stability   : experimental
Portability : POSIX

This module contains the tests for the knights module implemented as magic bitboards.
-}
{- FOURMOLU_DISABLE -}
{-|
-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -O -dsuppress-all -dno-suppress-type-signatures -fplugin=Test.Tasty.Inspection.Plugin #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use camelCase" #-}
{- FOURMOLU_ENABLE -}
module KnightsTest (knights_tests) where

import Bitboard
import Common
import Data.Vector
import Knights
import Test.Falsify.Predicate qualified as P
import Test.Tasty
import Test.Tasty.Falsify qualified as Falsify
import Test.Tasty.Inspection
import Gen

goal :: AttackBB
goal = getAttacks @KnightBB A1

reference :: AttackBB
reference = table ! square2Index A1

knights_tests :: TestTree
knights_tests = testGroup "Pawns tests" [knights_property, knights_inspection]

knights_inspection :: TestTree
knights_inspection = testGroup "Pawns inspection" [knights_specialization, knights_inline]

prop_get_attacks :: Falsify.Property ()
prop_get_attacks = do
    s <- Falsify.gen genSquare
    Falsify.assert $
        P.eq
            P..$ ("expected", table ! square2Index s)
            P..$ ("actual", getAttacks @KnightBB s)

{- attack_number :: Square -> Int
attack_number A1 = 2
attack_number A8 = 2
attack_number H1 = 2
attack_number H8 = 2
attack_number s
    | s' $&$ expect_3 /= 0 = 3
    | s' $&$ expect_4 /= 0 = 4
    | otherwise = 8
  where
    expect_3 = Prelude.foldr ($|$) 0 [fileA, fileH, rank1, rank8]
    expect_4 =
        ((fileA $|$ fileH) $&$ rank2Int R3 $&$ rank2Int R6)
            $|$ ((rank1 $|$ rank8) $&$ file2Int FC $&$ file2Int FF)
            $|$ squareMask B2
            $|$ squareMask G2
            $|$ squareMask B7
            $|$ squareMask G7
    expect_5 =
        ((fileA $|$ fileH) $&$ rank2Int R3 $&$ rank2Int R6)
            $|$ ((rank1 $|$ rank8) $&$ file2Int FC $&$ file2Int FF)
            $|$ squareMask B2
            $|$ squareMask G2
            $|$ squareMask B7
            $|$ squareMask G7
    s' = squareMask s -}

{- prop_attacks_number :: Falsify.Property ()
prop_attacks_number = do
    s <- Falsify.gen genSquare
    Falsify.assert $
        P.eq
            P..$ ("expected", attack_number s)
            P..$ ( "actual",
                   case getAttacks @KnightBB s of
                    AttackBB bb -> popCount bb
                 ) -}

knights_property :: TestTree
knights_property =
    testGroup
        "Pawns property"
        [ Falsify.testProperty "getAttacks" prop_get_attacks
        -- Falsify.testProperty "attacks number" prop_attacks_number
        ]

knights_specialization :: TestTree
knights_specialization =
    $( inspectTest
        ((hasNoTypeClasses 'goal) {testName = Just "getAttacks specialization"})
     )

knights_inline :: TestTree
knights_inline =
    $( inspectTest
        (('goal ==- 'reference) {testName = Just "getAttacks inlining"})
     )
