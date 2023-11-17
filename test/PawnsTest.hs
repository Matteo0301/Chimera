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
import QuickSpec.Internal.Testing (MonadTester (test))
import Test.Tasty
import Test.Tasty.Inspection
import Test.Tasty.QuickCheck as Qc
import Types

goal :: AttackBB
goal = getAttacks (Proxy :: Proxy (PawnBB 'White)) A1

reference :: AttackBB
reference = tableWhite ! square2Index A1

pawns_tests :: TestTree
pawns_tests = testGroup "Pawns tests" [pawns_property, pawns_inspection]

pawns_inspection :: TestTree
pawns_inspection = testGroup "Pawns inspection" [pawns_specialization, pawns_inline]

pawns_property :: TestTree
pawns_property =
    testGroup
        "Pawns property"
        [ Qc.testProperty "getAttacks" $
            \s -> getAttacks (Proxy :: Proxy (PawnBB 'White)) s == tableWhite ! square2Index s,
          Qc.testProperty "attacks number" $
            \s -> case getAttacks (Proxy :: Proxy (PawnBB 'White)) s of
                AttackBB bb -> popCount bb <= 2
        ]

pawns_specialization :: TestTree
pawns_specialization =
    $( inspectTest
        ((hasNoTypeClasses 'goal) {testName = Just "getAttacks specialization"})
     )

pawns_inline =
    $( inspectTest
        (('goal ==- 'reference) {testName = Just "getAttacks inlining"})
     )