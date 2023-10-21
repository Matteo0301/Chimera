module Main where

import Data.Text (pack, unpack)

main :: IO ()
main = print . unpack . pack $ "Hello, World!"

fib :: Int -> Int
fib 0 = 1
fib 1 = 1
fib n = (fib (n - 1)) + (fib (n - 2))