{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}

{-|
Module      : King.KingTH
Description : Template Haskell functions for King module
Copyright   : (c) 2023 Matteo Mariotti
License     : GNU GPL v.3
Maintainer  : matteomariotti0301@gmail.com
Stability   : experimental
Portability : POSIX

This module is a wrapper around King.Internal to provide Template Haskell functions.
-}
module King.KingTH where

import Bitboard
import Common
import Data.Vector
import King.Internal

table :: Vector AttackBB
table = $([|allocTable|])

instance Piece KingBB where
    {-# INLINE getAttacks #-}
    getAttacks :: Proxy KingBB -> Square -> AttackBB
    getAttacks _ s = table ! square2Index s
