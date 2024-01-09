{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}

{-|
Module      : Pawns.Internal
Description : Pawn representation using bitboards
Copyright   : (c) 2023 Matteo Mariotti
License     : GNU GPL v.3
Maintainer  : matteomariotti0301@gmail.com
Stability   : experimental
Portability : POSIX

This module contains the representation of pawns using bitboards, with also the relevant attack tables.
-}
module Pawns.Internal (PawnBBWrapped (..), showAttacks, allocTable) where

import Bitboard
import Bits
import Common
import Data.Array.Destination
import Data.Vector
import Prelude.Linear (($))
import Prelude hiding (($))

{-@ LIQUID "--no-termination" @-}

{- {-@ type Pop = {x:Int | x >= 0 && x<= 64} @-}
{-@ type Index = {x:Int | x >= 0 && x<= 63} @-}
{-@ measure bbPop :: Bitboard -> Pop @-} -}
-- {-@ data PawnBBWrapped a = PawnBBWrapped (bb :: {x:Bitboard | bbPop x <= 8}) @-}

newtype PawnBBWrapped (side :: SideToMove) = PawnBBWrapped Bitboard
    deriving (Eq, Show)

maskPawnAttack :: forall a. (GetSide a) => PawnBBWrapped a -> AttackBB
maskPawnAttack (PawnBBWrapped b) =
    let
        initial :: Int = bb b
        not_a_file :: Int -> Int
        not_a_file res = res $&$ complement fileA
        not_h_file res = res $&$ complement fileH
        attacks = AttackBB $ case getSide (Proxy :: Proxy a) of
            White -> not_a_file (initial `shiftR` 7) $|$ not_h_file (initial `shiftR` 9)
            Black -> not_h_file (initial `shiftL` 7) $|$ not_a_file (initial `shiftL` 9)
     in
        attacks

allocTable :: forall a. (GetSide a) => Vector AttackBB
allocTable = alloc 64 $ \newArr -> fromFunction fillFunction newArr
  where
    fillFunction :: Int -> AttackBB
    fillFunction i
        | i < 0 || i >= 64 = AttackBB 0
        | otherwise = maskPawnAttack @a (PawnBBWrapped (emptyBoard <<>> toEnum i))

showAttacks :: AttackBB -> Text
showAttacks (AttackBB bb) = showBits bb
