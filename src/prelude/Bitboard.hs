{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NoImplicitPrelude #-}

{-|
Module      : Bitboard
Description : Module for bitboards representation and operations
Copyright   : (c) 2023 Matteo Mariotti
License     : GNU GPL v.3
Maintainer  : matteomariotti0301@gmail.com
Stability   : experimental
Portability : POSIX

This module export the 'Bitboard' type and the functions to operate on its squares.
-}
module Bitboard
    ( Bitboard
    , Square (..)
    , File (..)
    , Rank (..)
    , trySet
    , showBits
    , getSquare
    , setSquare
    , (<<>>)
    , unsetSquare
    , getPopulation
    , emptyBoard
    , initialBoard
    , square2Index
    , bb2Word
    , maskFile
    , maskRank
    , file2Word
    , rank2Word
    ) where

import Bits

-- import Unsafe.Linear

-- import Data.Bits_LHAssumptions

import Common
import Prelude.Linear (($))
import System.Random
import Prelude hiding (($))

{-@ LIQUID "--counter-examples" @-}

{-@ type Pop = {x:Int | x >= 0 && x<= 64} @-}
{-@ type Index = {x:Int | x >= 0 && x<= 63} @-}
{-@ measure bbPop :: Bitboard -> Pop @-}

-- {-@ assume popCount :: Word64 -> Nat @-}
{-@ data Bitboard = Bitboard (bb::Word64) @-}
{-@ using (Bitboard) as ({x:Bitboard | bbPop x <= 32}) @-}

{-|
    The 'Bitboard' type is a newtype wrapper around 'Word64' that represents a bitboard.
    The bitboard is a 64-bit integer where each bit represents a square on the board.
    The least significant bit represents the square h1, the most significant bit represents the square a8.
    The bitboard is stored in little endian order, so the first 8 bits represent the first row of the board.
-}
newtype Bitboard where
    Bitboard :: Word64 %1 -> Bitboard
    deriving (Eq, Show, Ord)

{-|
    Represents the empty board.
-}
emptyBoard :: Bitboard
emptyBoard = Bitboard 0

{-|
    Represents the starting position.
-}
initialBoard :: Bitboard
initialBoard = Bitboard 0xFFFF00000000FFFF

{-|
    Converts a bitboard to its underlying 'Word64' representation.
-}
bb2Word :: Bitboard %1 -> Word64
bb2Word (Bitboard bb) = bb

{-@ assume getPopulation :: b:Bitboard -> {x:Pop | bbPop b = x} @-}

{-|
    Returns the number of squares occupied in the bitboard.
-}
getPopulation :: Bitboard %1 -> Int
getPopulation (Bitboard bb) = popCount bb

{-@ getSquare :: Bitboard -> Square -> Bool @-}

{-|
    Returns whether the square is set in the bitboard
-}
getSquare :: Bitboard %1 -> Square %1 -> Bool
getSquare (Bitboard bb) sq = testBit bb (square2Index sq)

-- {-@ setSquare :: x:Bitboard -> Square -> {y:Bitboard | getPopulation x == 32 => getPopulation y = 32 && getPopulation x < 32 => getPopulation y = getPopulation x + 1} @-}

{-|
    Sets the square in the bitboard. If the new bitboard has more that 32 squares occupied, returns the old one.
-}
setSquare :: Bitboard %1 -> Square %1 -> Bitboard
setSquare (Bitboard bb) sq = Bitboard $ setBit bb (square2Index sq)

{-@ assume unsetSquare :: x:Bitboard -> Square -> {y:Bitboard | bbPop x == 0 => bbPop y = 0 && bbPop x < 32 => bbPop y = bbPop x - 1} @-}

{-|
    Sets a certain square in the board as empty
-}
unsetSquare :: Bitboard %1 -> Square %1 -> Bitboard
unsetSquare (Bitboard bb) sq = Bitboard $ clearBit bb (square2Index sq)

{-@ (<<>>) :: Bitboard -> Square -> Bitboard @-}

{-|
    The operator version of 'setSquare'. It doesn't have a linear type because it is meant to be used mainly for testing (so it's easier to use through folds and maps).
-}
(<<>>) :: Bitboard -> Square -> Bitboard
bb <<>> i = setSquare bb i

{-@ trySet :: Word64 -> Bitboard @-}
{-# WARNING trySet "This function should be used only for testing" #-}

{-|
    Returns the bitboard if the population is between 0 and 32, otherwise returns the empty bitboard. This should only be used in tests.
-}
trySet :: Word64 -> Bitboard
trySet x = Bitboard $ clearRandomBits x

clearRandomBits :: Word64 -> Word64
clearRandomBits x =
    let
        g = mkStdGen $ fromIntegral x
        {-@ assume getRandom :: StdGen -> Index @-}
        getRandom :: StdGen -> Int
        getRandom g' = fst $ randomR (0, 63) g'
        {-@ lazy clearRandomBits' @-}
        clearRandomBits' 0 _ = 0
        clearRandomBits' i x'
            | i == 0 = 0
            | popCount x' <= 32 = x'
            | otherwise = clearRandomBits' (i - 1) $ clearBit x' (getRandom g)
     in
        clearRandomBits' (100 :: Int) x

{-|
    Shows the bitboard in a square representation, along with its numeric value
-}
showBits :: Word64 -> Text
showBits bb = showBits' 63
  where
    showBit :: Int -> Text
    showBit i =
        let
            b = testBit bb i
         in
            if b then "# " else ". "
    line :: Int -> Text
    line i = if i `mod` 8 == 0 then " " <> show (i `div` 8 + 1) <> "\n" else ""
    showBits' i
        | i < 0 || i >= 64 = ""
        | i == 0 =
            showBit i <> line i <> "a b c d e f g h\n" <> "Value: " <> show bb <> "\n"
        | otherwise = showBit i <> line i <> showBits' (i - 1)
