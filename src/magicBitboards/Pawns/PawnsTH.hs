{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}

{-|
Module      : Pawns.PawnsTH
Description : Template Haskell functions for Pawns module
Copyright   : (c) 2023 Matteo Mariotti
License     : GNU GPL v.3
Maintainer  : matteomariotti0301@gmail.com
Stability   : experimental
Portability : POSIX

This module is a wrapper around Pawns.Internal to provide Template Haskell functions.
-}
module Pawns.PawnsTH where

import Bitboard
import Common
import Data.Vector
import Pawns.Internal

tableWhite :: Vector AttackBB
tableWhite = $([|allocTable @'White|])

tableBlack :: Vector AttackBB
tableBlack = $([|allocTable @'Black|])

{-@ data PawnBB a = PawnBB (PawnBBWrapped a) @-}

{-|
    The basic type for pawn bitboards. It is a wrapper around 'Bitboard' that represents a bitboard with at most 8 bits set.
    The phantom type parameter specifies the side to move.
-}
newtype PawnBB (side :: SideToMove) = PawnBB (PawnBBWrapped side)
    deriving (Eq, Show)

instance (GetSide a) => Piece (PawnBB a) where
    {-# INLINE getAttacks #-}
    {-     {-# SPECIALIZE getAttacks :: 'White -> Square -> AttackBB #-}
        {-# SPECIALIZE getAttacks :: 'Black -> Square -> AttackBB #-} -}
    getAttacks :: Square -> AttackBB
    getAttacks s = case getSide (Proxy :: Proxy a) of
        White -> tableWhite ! square2Index s
        Black -> tableBlack ! square2Index s
