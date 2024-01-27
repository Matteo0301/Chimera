{-|
Module      : PawnsTest
Description : Pawns magic bitboards tests
Copyright   : (c) 2023-2024 Matteo Mariotti
License     : GNU GPL v.3
Stability   : experimental
Portability : POSIX

This module contains the tests for the pawns module implemented as magic bitboards.
-}
{- FOURMOLU_DISABLE -}
{-|
-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use camelCase" #-}
{- FOURMOLU_ENABLE -}
module PawnsTest (pawns_tests) where

import Bitboard
import Bits
import Common
import Data.Vector
import Gen
import Pawns
{- FOURMOLU_DISABLE -}
import qualified Test.Falsify.Predicate as P
import qualified Test.Tasty.Falsify as Falsify
{- FOURMOLU_ENABLE -}
import Data.Proxy
import InspectionTest
import Test.MuCheck.TestAdapter.TastyAdapter
import Test.Tasty

pawns_tests :: TestTree
pawns_tests = testGroup "Pawns tests" [pawns_property, pawns_inspection]

{-# ANN pawns_property "Test" #-}
pawns_property :: TestTree
pawns_property =
    testGroup
        "Pawns property"
        [ Falsify.testProperty "getAttacks" prop_get_attacks,
          Falsify.testProperty "attacks number" prop_attacks_number
        ]

prop_get_attacks :: Falsify.Property ()
prop_get_attacks = do
    s <- Falsify.gen genSquare
    Falsify.assert $
        P.eq
            P..$ ("expected", tableWhite ! square2Index s)
            P..$ ("actual", getAttacksProxy (Proxy :: Proxy WhitePawnBB) s)
    Falsify.assert $
        P.eq
            P..$ ("expected", tableBlack ! square2Index s)
            P..$ ("actual", getAttacksProxy (Proxy :: Proxy BlackPawnBB) s)

attack_number_white :: Square -> Int
attack_number_white s
    | s' $&$ rank8 /= 0 = 0
    | s' $&$ files /= 0 = 1
    | otherwise = 2
  where
    files = fileA $|$ fileH
    s' = squareMask s

attack_number_black :: Square -> Int
attack_number_black s
    | s' $&$ rank1 /= 0 = 0
    | s' $&$ files /= 0 = 1
    | otherwise = 2
  where
    files = fileA $|$ fileH
    s' = squareMask s

prop_attacks_number :: Falsify.Property ()
prop_attacks_number = do
    s <- Falsify.gen genSquare
    Falsify.assert $
        P.eq
            P..$ ("expected", attack_number_white s)
            P..$ ( "actual",
                   case getAttacksProxy (Proxy :: Proxy WhitePawnBB) s of
                    AttackBB bb -> popCount bb
                 )
    Falsify.assert $
        P.eq
            P..$ ("expected", attack_number_black s)
            P..$ ( "actual",
                   case getAttacksProxy (Proxy :: Proxy BlackPawnBB) s of
                    AttackBB bb -> popCount bb
                 )