{-|
Module      : King.KingTH
Description : Template Haskell functions for King module
Copyright   : (c) 2023-2024 Matteo Mariotti
License     : GNU GPL v.3
Stability   : experimental
Portability : POSIX

This module is a wrapper around King.Internal to provide Template Haskell functions.
-}
{- FOURMOLU_DISABLE -}
{-|
-}
{- FOURMOLU_ENABLE -}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module King.KingTH where

import Bitboard
import Common
import Data.Vector
import King.Internal

table :: Vector AttackBB
table = $([|allocTable|])

instance Piece KingBB where
    {-# INLINE getAttacks #-}
    getAttacks :: Square -> AttackBB
    getAttacks s = table ! square2Index s
