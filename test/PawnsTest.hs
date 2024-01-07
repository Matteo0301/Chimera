{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -O -dsuppress-all -dno-suppress-type-signatures -fplugin=Test.Tasty.Inspection.Plugin #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use camelCase" #-}

module PawnsTest (pawns_tests) where

import Bitboard
import Bits
import Common
import Data.Vector
import Pawns
import Test.Falsify.Predicate qualified as P
import Test.Tasty
import Test.Tasty.Falsify qualified as Falsify
import Test.Tasty.Inspection
import Types

goal :: AttackBB
goal = getAttacks @(PawnBB 'White) A1

reference :: AttackBB
reference = tableWhite ! square2Index A1

pawns_tests :: TestTree
pawns_tests = testGroup "Pawns tests" [pawns_property, pawns_inspection]

pawns_inspection :: TestTree
pawns_inspection = testGroup "Pawns inspection" [pawns_specialization, pawns_inline]

prop_get_attacks :: Falsify.Property ()
prop_get_attacks = do
    s <- Falsify.gen genSquare
    Falsify.assert $
        P.eq
            P..$ ("expected", tableWhite ! square2Index s)
            P..$ ("actual", getAttacks @(PawnBB 'White) s)

prop_attacks_number :: Falsify.Property ()
prop_attacks_number = do
    s <- Falsify.gen genSquare
    Falsify.assert $
        P.ge
            P..$ ("expected", 2)
            P..$ ( "actual",
                   case getAttacks @(PawnBB 'White) s of
                    AttackBB bb -> popCount bb
                 )

pawns_property :: TestTree
pawns_property =
    testGroup
        "Pawns property"
        [ Falsify.testProperty "getAttacks" prop_get_attacks,
          Falsify.testProperty "attacks number" prop_attacks_number
        ]

pawns_specialization :: TestTree
pawns_specialization =
    $( inspectTest
        ((hasNoTypeClasses 'goal) {testName = Just "getAttacks specialization"})
     )

pawns_inline :: TestTree
pawns_inline =
    $( inspectTest
        (('goal ==- 'reference) {testName = Just "getAttacks inlining"})
     )
