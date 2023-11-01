{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
module BitboardTest (bb_tests) where
import Test.Tasty.Runners (TestTree(TestGroup))
import Bitboard
import Test.Tasty.QuickCheck as QC
import Test.Tasty
import Test.Tasty.HUnit


instance Arbitrary Bitboard where
    arbitrary :: Gen Bitboard
    arbitrary = do
        bb <- choose (0, 2 ^ (64::Int) - 1)
        suchThat (return $ trySet bb) (\x -> getPopulation x <= 32)
    
    shrink :: Bitboard -> [Bitboard]
    shrink bb
            | bb == emptyBoard = []
            | otherwise = [unsetSquare bb x | x <- [A1 .. H8], getSquare bb x]

instance Arbitrary Square where
    arbitrary :: Gen Square
    arbitrary = do
        s <- choose (0, 32)
        return $ toEnum s

bb_tests :: TestTree
bb_tests = TestGroup "Bitboard tests" [set_get_bit, population_tests]

set_get_bit :: TestTree
set_get_bit =
    testGroup
        "Set/unset squares"
        [ QC.testProperty "set . get" $
            \x -> getSquare (setSquare emptyBoard x) x
            , QC.testProperty "unset . set" $
            \x -> unsetSquare (setSquare emptyBoard x) x == emptyBoard
            , QC.testProperty "set . unset" $
            \x -> unsetSquare emptyBoard x == emptyBoard
            , QC.testProperty "getPopulation . set" $
            \x -> getPopulation (setSquare emptyBoard x) == 1
        ]

population_tests :: TestTree
population_tests =
    testGroup
        "Population tests"
        [ testCase "Population: empty" $
            getPopulation emptyBoard @?= 0
            ,testCase "Population: A1" $
            getPopulation (emptyBoard<<>>A1) @?= 1
            ,testCase "Population: D5" $
            getPopulation (emptyBoard<<>>D5) @?= 1
            ,testCase "Population: white pawns" $
            getPopulation (emptyBoard <<>> A2 <<>> B2 <<>> C2 <<>> D2 <<>> E2 <<>> F2 <<>> G2 <<>> H2) @?= 8
            , QC.testProperty "getPopulation<=32" $
            \x -> getPopulation x <= 32
        ]