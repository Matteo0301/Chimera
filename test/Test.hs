{-|
Module      : !!! INSERT HASKELL MODULE NAME !!!
Description : !!! INSERT MODULE SHORT DESCRIPTION !!!
Copyright   : (c) 2023 Matteo Mariotti
License     : GNU GPL v.3
Maintainer  : matteomariotti0301@gmail.com
Stability   : experimental
Portability : POSIX

!!! INSERT MODULE LONG DESCRIPTION !!!
-}
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.Ingredients.Rerun
import Test.Tasty.QuickCheck as QC

main = defaultMainWithRerun tests

tests :: TestTree
tests = testGroup "Tests" [properties, unitTests]

properties :: TestTree
properties = testGroup "Properties" [qcProps]

qcProps =
    testGroup
        "(checked by QuickCheck)"
        [ QC.testProperty "Fermat's little theorem"
            $ \x -> ((x :: Integer) ^ 7 - x) `mod` 7 == 0
        ]

unitTests =
    testGroup
        "Unit tests"
        [ testCase "List comparison (different length)"
            $ [1, 2, 3]
            `compare` [1, 2]
            @?= GT
        ]
