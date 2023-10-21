import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.Ingredients.Rerun
import Test.Tasty.QuickCheck as QC

import Data.List
import Data.Ord

main = defaultMainWithRerun tests

tests :: TestTree
tests = testGroup "Tests" [properties, unitTests]

properties :: TestTree
properties = testGroup "Properties" [qcProps]

qcProps =
    testGroup
        "(checked by QuickCheck)"
        [ QC.testProperty "sort == sort . reverse" $
            \list -> sort (list :: [Int]) == sort (reverse list),
          QC.testProperty "Fermat's little theorem" $
            \x -> ((x :: Integer) ^ 7 - x) `mod` 7 == 0
        ]

unitTests =
    testGroup
        "Unit tests"
        [ testCase "List comparison (different length)" $
            [1, 2, 3] `compare` [1, 2] @?= GT
        ]