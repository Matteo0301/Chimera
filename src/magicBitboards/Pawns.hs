{-|
Module      : Pawns
Description : Pawn representation using bitboards
Copyright   : (c) 2023-2024 Matteo Mariotti
License     : GNU GPL v.3
Stability   : experimental
Portability : POSIX

This module contains the representation of pawns using bitboards, with also the relevant attack tables.
-}
{- FOURMOLU_DISABLE -}
{-|
-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{- FOURMOLU_ENABLE -}
module Pawns (module P) where

import Bitboard
import Pawns.Internal as P hiding (allocTable)
import Pawns.PawnsTH as P
