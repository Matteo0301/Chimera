{-# OPTIONS_GHC -Wno-orphans #-}

module Types (Bitboard, Square (..)) where

import Bitboard
import Test.Tasty.QuickCheck

instance Arbitrary Bitboard where
    arbitrary :: Gen Bitboard
    arbitrary = do
        -- bb <- choose (0, 2 ^ (64 :: Int) - 1)
        suchThat (return $ emptyBoard) (\x -> population x <= 32)

    shrink :: Bitboard -> [Bitboard]
    shrink bb
        | bb == emptyBoard = []
        | otherwise = [unsetSquare bb x | x <- [A1 .. H8], getSquare bb x]

instance Arbitrary Square where
    arbitrary :: Gen Square
    arbitrary = do
        s <- choose (0, 32)
        return $ toEnum s
