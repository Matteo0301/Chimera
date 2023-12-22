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
    -- , trySet
    , showBits
    , getSquare
    , setSquare
    , (<<>>)
    , unsetSquare
    , population
    , emptyBoard
    , initialBoard
    , square2Index
    , bb2Int
    , maskFile
    , maskRank
    , file2Word
    , rank2Word
    , bbPop
    ) where

import Bits

import Common
import Prelude.Linear (($))
import Prelude hiding (($))

{-@ LIQUID "--counter-examples" @-}
{-@ LIQUID "--reflection" @-}
{-@ LIQUID "--ple" @-}
{-@ LIQUID "--prune-unsorted" @-}

{-@ type Pop = {x:Int | x >= 0 && x<= 64} @-}
{-@ type Index = {x:Int | x >= 0 && x<= 63} @-}

{-@ data Bitboard' = Bitboard (bb::Int) @-}

{-|
    The 'Bitboard' type is a newtype wrapper around 'Int' that represents a bitboard.
    The bitboard is a 64-bit integer where each bit represents a square on the board.
    The least significant bit represents the square h1, the most significant bit represents the square a8.
    The bitboard is stored in little endian order, so the first 8 bits represent the first row of the board.
-}
newtype Bitboard' where
    Bitboard :: Int %1 -> Bitboard'
    deriving (Eq, Show, Ord)

{-@ type Bitboard = {a:Bitboard' | bbPop a <= 32} @-}
-- {-@ type Bitboard = Bitboard' @-}
type Bitboard = Bitboard'

{-@ emptyBoard :: Bitboard @-}

{-|
    Represents the empty board.
-}
emptyBoard :: Bitboard
emptyBoard = Bitboard 0

{-@ initialBoard :: Bitboard @-}

{-|
    Represents the starting position.
-}
initialBoard :: Bitboard
initialBoard = Bitboard (-0x0000FFFFFFFF0001)

{-@ inline bb2Int @-}

{-|
    Converts a bitboard to its underlying 'Int' representation.
-}
bb2Int :: Bitboard' %1 -> Int
bb2Int (Bitboard bb) = bb

{-@ inline bbPop @-}
bbPop :: Bitboard' -> Int
bbPop (Bitboard bb) = pop bb

{-|
    Returns the number of squares occupied in the bitboard.
-}
population :: Bitboard %1 -> Int
population (Bitboard bb) = popCount bb

-- {-@ getSquare :: Bitboard -> Square -> Bool @-}

{-|
    Returns whether the square is set in the bitboard
-}
getSquare :: Bitboard %1 -> Square %1 -> Bool
getSquare (Bitboard bb) sq = testBit bb (square2Index sq)

-- {-@ setSquare :: x:Bitboard -> Square -> {y:Bitboard | population x == 32 => population y = 32 && population x < 32 => population y = population x + 1} @-}

{-|
    Sets the square in the bitboard. If the new bitboard has more that 32 squares occupied, returns the old one.
-}
setSquare :: Bitboard %1 -> Square %1 -> Bitboard
setSquare (Bitboard bb) sq = Bitboard $ setBit bb (square2Index sq)

-- {-@ assume unsetSquare :: x:Bitboard -> Square -> {y:Bitboard | bbPop x == 0 => bbPop y = 0 && bbPop x < 32 => bbPop y = bbPop x - 1} @-}

{-|
    Sets a certain square in the board as empty
-}
unsetSquare :: Bitboard %1 -> Square %1 -> Bitboard
unsetSquare (Bitboard bb) sq = Bitboard $ clearBit bb (square2Index sq)

-- {-@ (<<>>) :: Bitboard -> Square -> Bitboard @-}

{-|
    The operator version of 'setSquare'. It doesn't have a linear type because it is meant to be used mainly for testing (so it's easier to use through folds and maps).
-}
(<<>>) :: Bitboard -> Square -> Bitboard
bb <<>> i = setSquare bb i

{-|
    Shows the bitboard in a square representation, along with its numeric value
-}
showBits :: Int -> Text
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
