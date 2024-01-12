{-|
Module      : Knights
Description : Knight representation using bitboards
Copyright   : (c) 2023-2024 Matteo Mariotti
License     : GNU GPL v.3
Stability   : experimental
Portability : POSIX

This module contains the representation of knight using bitboards, with also the relevant attack tables.
-}
{- FOURMOLU_DISABLE -}
{-|
-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
module Knights (module P) where

import Bitboard
import Knights.Internal as P hiding (KnightBBWrapped (..), allocTable)
import Knights.KnightsTH as P
