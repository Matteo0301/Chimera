{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}

{-|
Module      : Pawns
Description : Pawn representation using bitboards
Copyright   : (c) 2023 Matteo Mariotti
License     : GNU GPL v.3
Maintainer  : matteomariotti0301@gmail.com
Stability   : experimental
Portability : POSIX

This module contains the representation of pawns using bitboards, with also the relevant attack tables.
-}
module Pawns (PawnBB (..)) where

import Prelude.Linear (($))
import Prelude hiding (($))

import Bitboard
import Bits
import Common

{- {-@ type Pop = {x:Int | x >= 0 && x<= 64} @-}
{-@ type Index = {x:Int | x >= 0 && x<= 63} @-}
{-@ measure bbPop :: Bitboard -> Pop @-} -}
{-@ data PawnBB a = PawnBB (bb :: {x:Bitboard | bbPop x <= 8}) @-}

{-|
    The basic type for pawn bitboards. It is a wrapper around 'Bitboard' that represents a bitboard with at most 8 bits set.
    The phantom type parameter specifies the side to move.
-}
newtype PawnBB (side :: SideToMove) = PawnBB Bitboard deriving (Eq, Show)

instance PieceBB (PawnBB 'White) where
    getAttacks = error "Not implemented"

instance PieceBB (PawnBB 'Black) where
    getAttacks = error "Not implemented"

maskPawnAttack :: forall a. (GetSide a) => PawnBB a -> AttackBB
maskPawnAttack (PawnBB bb) =
    let
        initial :: Word64 = bb2Word bb
        not_a_file :: Word64 -> Word64
        not_a_file res = res .&. complement (file2Word FA)
        not_h_file res = res .&. complement (file2Word FH)
        attacks = AttackBB $ case getSide (Proxy :: Proxy a) of
            White -> not_a_file (initial `shiftL` 7) .|. not_h_file (initial `shiftL` 9)
            Black -> not_h_file (initial `shiftR` 7) .|. not_a_file (initial `shiftR` 9)
     in
        attacks

showAttacks :: AttackBB -> Text
showAttacks (AttackBB bb) = showBits bb