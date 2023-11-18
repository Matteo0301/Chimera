{-# OPTIONS_GHC -Wno-unused-imports #-}

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
module Pawns (module P) where

import Bitboard
import Pawns.Internal as P hiding (PawnBBWrapped (..), allocTable)
import Pawns.PawnsTH as P
