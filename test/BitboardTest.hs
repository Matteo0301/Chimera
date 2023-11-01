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
            \x -> getSquare (setSquare mempty x) x
            , QC.testProperty "unset . set" $
            \x -> unsetSquare (setSquare mempty x) x == mempty
            , QC.testProperty "set . unset" $
            \x -> unsetSquare mempty x == mempty
            , QC.testProperty "getPopulation . set" $
            \x -> getPopulation (setSquare mempty x) == 1
        ]

population_tests :: TestTree
population_tests =
    testGroup
        "Population tests"
        [ testCase "Population: empty" $
            getPopulation mempty @?= 0
            ,testCase "Population: A1" $
            getPopulation (mempty<<>>A1) @?= 1
            ,testCase "Population: D5" $
            getPopulation (mempty<<>>D5) @?= 1
            ,testCase "Population: white pawns" $
            getPopulation (mempty <<>> A2 <<>> B2 <<>> C2 <<>> D2 <<>> E2 <<>> F2 <<>> G2 <<>> H2) @?= 8
        ]