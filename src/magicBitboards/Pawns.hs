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
module Pawns (PawnBB (..), WhitePawnBB, BlackPawnBB) where

import Bitboard

{-@ data PawnBB = PawnBB (bb :: {x:Bitboard | getPopulation x <= 8}) @-}
newtype PawnBB = PawnBB Bitboard

{-@ type WhitePawnBB = {x:PawnBB | not (getSquare bb A1) && not (getSquare bb B1) && not (getSquare bb C1) && not (getSquare bb D1) && not (getSquare bb E1) && not (getSquare bb F1) && not (getSquare bb G1) && not (getSquare bb H1)} @-}
type WhitePawnBB = PawnBB

{-@ type BlackPawnBB = {x:PawnBB | not (getSquare bb A8) && not (getSquare bb B8) && not (getSquare bb C8) && not (getSquare bb D8) && not (getSquare bb E8) && not (getSquare bb F8) && not (getSquare bb G8) && not (getSquare bb H8)} @-}
type BlackPawnBB = PawnBB
