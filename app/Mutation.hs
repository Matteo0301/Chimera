module Main where

import Test.MuCheck
import Test.MuCheck.TestAdapter
import Test.MuCheck.TestAdapter.TastyAdapter

checkFile :: String -> Bool -> IO ()
checkFile file log = do
    putStrLn $ "Checking " ++ file
    (msum, _tsum) <- mucheck (toRun file :: TastyRun) []
    print msum
    when log $ do
        putStrLn "Log:"
        print _tsum

main :: IO ()
main = do
    checkFile "test/BitboardTest.hs" False
    checkFile "test/KingTest.hs" False
    checkFile "test/KnightsTest.hs" False
    checkFile "test/PawnsTest.hs" False