-- \|
-- Module      : !!! INSERT HASKELL MODULE NAME !!!
-- Description : !!! INSERT MODULE SHORT DESCRIPTION !!!
-- Copyright   : (c) 2023 Matteo Mariotti
-- License     : GNU GPL v.3
-- Maintainer  : matteomariotti0301@gmail.com
-- Stability   : experimental
-- Portability : POSIX
--
-- !!! INSERT MODULE LONG DESCRIPTION !!!

import BitboardBench
import Test.Tasty.Bench

-- Our benchmark harness.
main :: IO ()
main =
    defaultMain
        [ bitboard_benches
        ]
