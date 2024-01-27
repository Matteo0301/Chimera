{-|
Module      : InspectionTest
Description : Confining inspection testing inside a standalone module
Copyright   : (c) 2023-2024 Matteo Mariotti
License     : GNU GPL v.3
Stability   : experimental
Portability : POSIX

This module contains the tests for inspecting the properties of the implementation.
-}
{- FOURMOLU_DISABLE -}
{-|
-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -O -dsuppress-all -dno-suppress-type-signatures -fplugin=Test.Tasty.Inspection.Plugin #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use camelCase" #-}
{- FOURMOLU_ENABLE -}

module InspectionTest (king_inspection, knights_inspection, pawns_inspection) where

import Common
import Data.Proxy
import Data.Vector
import King qualified as KI
import Knights qualified as KN
import Pawns qualified as PN
import Test.Tasty
import Test.Tasty.Inspection

king_inspection :: TestTree
king_inspection = testGroup "King inspection" [king_specialization, king_inline]

king_goal :: AttackBB
king_goal = getAttacksProxy (Proxy :: Proxy KI.KingBB) A1

king_reference :: AttackBB
king_reference = KI.table ! square2Index A1

king_specialization :: TestTree
king_specialization =
    $( inspectTest
        ((hasNoTypeClasses 'king_goal) {testName = Just "getAttacks specialization"})
     )

king_inline :: TestTree
king_inline =
    $( inspectTest
        (('king_goal ==- 'king_reference) {testName = Just "getAttacks inlining"})
     )

knights_inspection :: TestTree
knights_inspection = testGroup "Knights inspection" [knights_specialization, knights_inline]

knights_goal :: AttackBB
knights_goal = getAttacks @KN.KnightBB A1

knights_reference :: AttackBB
knights_reference = KN.table ! square2Index A1

knights_specialization :: TestTree
knights_specialization =
    $( inspectTest
        ((hasNoTypeClasses 'knights_goal) {testName = Just "getAttacks specialization"})
     )

knights_inline :: TestTree
knights_inline =
    $( inspectTest
        (('knights_goal ==- 'knights_reference) {testName = Just "getAttacks inlining"})
     )

pawns_goal :: AttackBB
pawns_goal = getAttacks @(PN.PawnBB 'White) A1

pawns_reference :: AttackBB
pawns_reference = PN.tableWhite ! square2Index A1

pawns_inspection :: TestTree
pawns_inspection = testGroup "Pawns inspection" [pawns_specialization, pawns_inline]

pawns_specialization :: TestTree
pawns_specialization =
    $( inspectTest
        ((hasNoTypeClasses 'pawns_goal) {testName = Just "getAttacks specialization"})
     )

pawns_inline :: TestTree
pawns_inline =
    $( inspectTest
        (('pawns_goal ==- 'pawns_reference) {testName = Just "getAttacks inlining"})
     )
