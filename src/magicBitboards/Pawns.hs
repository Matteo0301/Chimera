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

import Bitboard
import Common

{-@ data PawnBB a = PawnBB (bb :: {x:Bitboard | getPopulation x <= 8}) @-}

{-|
    The basic type for pawn bitboards. It is a wrapper around 'Bitboard' that represents a bitboard with at most 8 bits set.
    The phantom type parameter specifies the side to move.
-}
newtype PawnBB (side :: SideToMove) = PawnBB Bitboard

instance PieceBB (PawnBB 'White) where
    getAttacks = error "Not implemented"
    side = White

instance PieceBB (PawnBB 'Black) where
    getAttacks = error "Not implemented"
    side = Black