{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}

{-|
Module      : Knights.KnightsTH
Description : Template Haskell functions for Knights module
Copyright   : (c) 2023 Matteo Mariotti
License     : GNU GPL v.3
Maintainer  : matteomariotti0301@gmail.com
Stability   : experimental
Portability : POSIX

This module is a wrapper around Knights.Internal to provide Template Haskell functions.
-}
module Knights.KnightsTH where

import Bitboard
import Common
import Data.Vector
import Knights.Internal

tableWhite :: Vector AttackBB
tableWhite = $([|allocTable|])

instance Piece KnightBB where
    {-# INLINE getAttacks #-}
    getAttacks :: Proxy KnightBB -> Square -> AttackBB
    getAttacks _ s = tableWhite ! square2Index s
