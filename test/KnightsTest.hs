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
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use camelCase" #-}
{- FOURMOLU_ENABLE -}
module KnightsTest (knights_tests) where

import Bitboard
import Common
import Data.Vector
import Gen
import InspectionTest
import Knights
{- FOURMOLU_DISABLE -}
import qualified Test.Falsify.Predicate as P
import qualified Test.Tasty.Falsify as Falsify
{- FOURMOLU_ENABLE -}
import Data.Proxy
import Test.MuCheck.TestAdapter.TastyAdapter
import Test.Tasty

knights_tests :: TestTree
knights_tests = testGroup "Knights tests" [knights_property, knights_inspection]

{-# ANN knights_property "Test" #-}
knights_property :: TestTree
knights_property =
    testGroup
        "Knights property"
        [ Falsify.testProperty "getAttacks" prop_get_attacks
        -- Falsify.testProperty "attacks number" prop_attacks_number
        ]

prop_get_attacks :: Falsify.Property ()
prop_get_attacks = do
    s <- Falsify.gen genSquare
    Falsify.assert $
        P.eq
            P..$ ("expected", table ! square2Index s)
            P..$ ("actual", getAttacksProxy (Proxy :: Proxy KnightBB) s)

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
