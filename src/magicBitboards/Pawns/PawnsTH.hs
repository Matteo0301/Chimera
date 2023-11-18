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

instance (GetSide a) => Piece (PawnBB a) where
    {-# INLINE getAttacks #-}
    {-# SPECIALIZE getAttacks :: Proxy (PawnBB 'White) -> Square -> AttackBB #-}
    {-# SPECIALIZE getAttacks :: Proxy (PawnBB 'Black) -> Square -> AttackBB #-}
    getAttacks :: (GetSide a) => Proxy (PawnBB a) -> Square -> AttackBB
    getAttacks _ s = case getSide (Proxy :: Proxy a) of
        White -> tableWhite ! square2Index s
        Black -> tableBlack ! square2Index s