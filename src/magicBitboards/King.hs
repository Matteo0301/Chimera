{-|
Module      : King
Description : Knight representation using bitboards
Copyright   : (c) 2023 Matteo Mariotti
License     : GNU GPL v.3
Maintainer  : matteomariotti0301@gmail.com
Stability   : experimental
Portability : POSIX

This module contains the representation of king using bitboards, with also the relevant attack tables.
-}
module King (module P) where

import King.Internal as P hiding (allocTable)
import King.KingTH as P
