{-|
Module      : Gen
Description : Generation of random values for testing purposes
Copyright   : (c) 2023-2024 Matteo Mariotti
License     : GNU GPL v.3
Stability   : experimental
Portability : POSIX

This module implements the generation of random values for the types defined in the project. It uses the Falsify library.
-}
{- FOURMOLU_DISABLE -}
{-|
-}
{-# OPTIONS_GHC -Wno-orphans #-}
{- FOURMOLU_ENABLE -}

module Gen (Bitboard, Square (..), genBitboard, genSquare) where

import Bitboard
import Test.Falsify.Generator qualified as Generator
import Test.Falsify.Range

genSquare :: Generator.Gen Square
genSquare = do
    Generator.inRange (enum (A1, H8))

genBitboard :: Generator.Gen Bitboard
genBitboard = do
    l <- Generator.list (between (0, 32)) genSquare
    return $ fromList l
