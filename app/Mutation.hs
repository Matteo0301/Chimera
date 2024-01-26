module Main where

import Test.MuCheck
import Test.MuCheck.TestAdapter
import Test.MuCheck.TestAdapter.TastyAdapter

checkFile :: String -> IO ()
checkFile file = do
    (msum, _tsum) <- mucheck (toRun file :: TastyRun) []
    print msum

main :: IO ()
main = do
    checkFile "test/BitboardTest.hs"