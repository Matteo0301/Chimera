{-|
Module      : Knights.KnightsTH
Description : Template Haskell functions for Knights module
Copyright   : (c) 2023-2024 Matteo Mariotti
License     : GNU GPL v.3
Stability   : experimental
Portability : POSIX

This module is a wrapper around Knights.Internal to provide Template Haskell functions.
-}
{- FOURMOLU_DISABLE -}
{-|
-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{- FOURMOLU_ENABLE -}
module Knights.KnightsTH where

import Bitboard
import Common (AttackBB, Piece (..))
import Data.Vector
import Knights.Internal

table :: Vector AttackBB
table = $([|allocTable|])

instance Piece KnightBB where
    {-# INLINE getAttacks #-}
    getAttacks :: Square -> AttackBB
    getAttacks s = table ! square2Index s
