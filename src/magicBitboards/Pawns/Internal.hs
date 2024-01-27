{-|
Module      : Pawns.Internal
Description : Pawn representation using bitboards
Copyright   : (c) 2023-2024 Matteo Mariotti
License     : GNU GPL v.3
Stability   : experimental
Portability : POSIX

This module contains the representation of pawns using bitboards, with also the relevant attack tables.
-}
{- FOURMOLU_DISABLE -}
{-|
-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Pawns.Internal (PawnBB (..), showAttacks, allocTable, WhitePawnBB, BlackPawnBB) where

import Bitboard
import Bits
import Common
import Data.Array.Destination
import Data.Vector
import Prelude.Linear (($))
import Prelude hiding (($))

{- {-@ type Pop = {x:Int | x >= 0 && x<= 64} @-}
{-@ type Index = {x:Int | x >= 0 && x<= 63} @-}
{-@ measure bbPop :: Bitboard -> Pop @-} -}
{-@ type KingBB = {x:Bitboard | bbPop x <= 8}  @-}

{-|
    The basic type for pawn bitboards. It is a wrapper around 'Bitboard' that represents a bitboard with at most 8 bits set.
    The phantom type parameter specifies the side to move.
-}
newtype PawnBB (side :: SideToMove) = PawnBB Bitboard
    deriving (Eq, Show)

type WhitePawnBB = PawnBB 'White
type BlackPawnBB = PawnBB 'Black

maskPawnAttack :: forall a. (GetSide a) => PawnBB a -> AttackBB
maskPawnAttack (PawnBB b) =
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
        | otherwise = maskPawnAttack @a (PawnBB (emptyBoard <<>> index2Square i))

showAttacks :: AttackBB -> Text
showAttacks (AttackBB bb) = showBits bb
