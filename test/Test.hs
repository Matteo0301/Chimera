-- \|
-- Description : Main test module
-- Copyright   : (c) 2023 - 2024 Matteo Mariotti
-- License     : GNU GPL v.3
-- Maintainer  : matteomariotti0301@gmail.com
-- Stability   : experimental
-- Portability : POSIX
--
-- This is the module for the test suite of the project.

import BitboardTest
import KingTest (king_tests)
import KnightsTest (knights_tests)
import PawnsTest (pawns_tests)
import Test.Tasty
import Test.Tasty.Ingredients (composeReporters)
import Test.Tasty.Ingredients.Basic
import Test.Tasty.Ingredients.Rerun
import Test.Tasty.Runners.AntXML

main :: IO ()
main =
    defaultMainWithIngredients
        [ rerunningTests
            (composeReporters antXMLRunner consoleTestReporter : defaultIngredients)
        ]
        tests

tests :: TestTree
tests = testGroup "Tests" [bb_tests, pawns_tests, king_tests, knights_tests]
