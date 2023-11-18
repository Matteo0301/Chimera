{-# OPTIONS_GHC -Wno-unused-imports #-}

{-|
Module      : Knights
Description : Knight representation using bitboards
Copyright   : (c) 2023 Matteo Mariotti
License     : GNU GPL v.3
Maintainer  : matteomariotti0301@gmail.com
Stability   : experimental
Portability : POSIX

This module contains the representation of knight using bitboards, with also the relevant attack tables.
-}
module Knights (module P) where

import Bitboard
import Knights.Internal as P hiding (KnightBBWrapped (..), allocTable)
import Knights.KnightsTH as P
