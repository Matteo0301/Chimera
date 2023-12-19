{-|
Module      : Main
Description : !!! INSERT MODULE SHORT DESCRIPTION !!!
Copyright   : (c) 2023 Matteo Mariotti
License     : GNU GPL v.3
Maintainer  : matteomariotti0301@gmail.com
Stability   : experimental
Portability : POSIX

!!! INSERT MODULE LONG DESCRIPTION !!!
-}
module Main where

import Data.Text (pack, unpack)

main :: IO ()
main = print (pop 0xFFFF00000000FFFF)

pop :: Int -> Int
pop 0 = 0
pop x = (x `mod` 2) + pop (x `div` 2)