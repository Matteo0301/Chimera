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
import Criterion.Main

-- The function we're benchmarking.
fib m
    | m < 0 = error "negative!"
    | otherwise = go m
  where
    go 0 = 0
    go 1 = 1
    go n = go (n - 1) + go (n - 2)

-- Our benchmark harness.
main =
    defaultMain
        [ bgroup
            "fib"
            [ bench "1" $ whnf fib 1,
              bench "5" $ whnf fib 5
            ]
        ]
