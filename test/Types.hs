{-# OPTIONS_GHC -Wno-orphans #-}

module Types (Bitboard, Square (..), genBitboard, genSquare) where

import Bitboard
import Test.Falsify.Generator qualified as Generator
import Test.Falsify.Range

genSquare :: Generator.Gen Square
genSquare = do
    Generator.inRange (enum (A1, H8))

genBitboard :: Generator.Gen Bitboard
genBitboard = do
    l <- Generator.list (between (0, 32)) genSquare
    return $ foldl setSquare emptyBoard l
