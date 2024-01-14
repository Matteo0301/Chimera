{-|
Module      : Pawns.PawnsTH
Description : Template Haskell functions for Pawns module
Copyright   : (c) 2023-2024 Matteo Mariotti
License     : GNU GPL v.3
Stability   : experimental
Portability : POSIX

This module is a wrapper around Pawns.Internal to provide Template Haskell functions.
-}
{- FOURMOLU_DISABLE -}
{-|
-}
{- FOURMOLU_ENABLE -}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}
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
    {-     {-# SPECIALIZE getAttacks :: 'White -> Square -> AttackBB #-}
        {-# SPECIALIZE getAttacks :: 'Black -> Square -> AttackBB #-} -}
    getAttacks :: Square -> AttackBB
    getAttacks s = case getSide (Proxy :: Proxy a) of
        White -> tableWhite ! square2Index s
        Black -> tableBlack ! square2Index s
