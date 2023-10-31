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
{-# LANGUAGE OverloadedStrings #-}
module Bitboard (
    Bitboard(..)
    , Square(..)
    , showBits
    , getSquare
    , setSquare
    , (<<>>)
    , unsetSquare
    , getPopulation
) where

import Data.Bits ( Bits (..) )


{-@ LIQUID "--counter-examples" @-}
{-@ LIQUID "--diff" @-}

{-@ type Board = Word64 @-}

{-@ data Bitboard = Bitboard (bb :: {x:Word64 | getPopulation x >= 0 || getPopulation x <= 32 }) @-}
{-|
    The 'Bitboard' type is a newtype wrapper around 'Word64' that represents a bitboard.
    The bitboard is a 64-bit integer where each bit represents a square on the board.
    The least significant bit represents the square h1, the most significant bit represents the square a8.
    The bitboard is stored in little endian order, so the first 8 bits represent the first row of the board.-}
{-|
    The 'Bitboard' type is a newtype wrapper around 'Word64' that represents a bitboard.
    The bitboard is a 64-bit integer where each bit represents a square on the board.
    The least significant bit represents the square h1, the most significant bit represents the square a8.
    The bitboard is stored in little endian order, so the first 8 bits represent the first row of the board.-}
newtype Bitboard = Bitboard { bb :: Word64 }
    deriving (Eq)

instance Semigroup Bitboard where
    (<>) :: Bitboard -> Bitboard -> Bitboard
    (Bitboard bb1) <> (Bitboard bb2) = Bitboard $ bb1 .|. bb2

instance Monoid Bitboard where
    mempty :: Bitboard
    mempty = Bitboard 0

{-|
    The 'Square' type represents the index of a square on the board.-}
data Square = H1 | G1 | F1 | E1 | D1 | C1 | B1 | A1
            | H2 | G2 | F2 | E2 | D2 | C2 | B2 | A2
            | H3 | G3 | F3 | E3 | D3 | C3 | B3 | A3
            | H4 | G4 | F4 | E4 | D4 | C4 | B4 | A4
            | H5 | G5 | F5 | E5 | D5 | C5 | B5 | A5
            | H6 | G6 | F6 | E6 | D6 | C6 | B6 | A6
            | H7 | G7 | F7 | E7 | D7 | C7 | B7 | A7
            | H8 | G8 | F8 | E8 | D8 | C8 | B8 | A8
            deriving (Eq, Show, Enum)


{-@ assume getPopulation :: Bitboard -> {x:Int | x>=0 || x<=64} @-}
{-@ measure getPopulation :: Bitboard -> Int @-}
getPopulation :: Bitboard -> Int
getPopulation (Bitboard bb) = popCount bb

{-@ getSquare :: Bitboard -> Square -> Bool @-}
{-|
    Returns whether the square is set in the bitboard-}
getSquare :: Bitboard -> Square -> Bool
getSquare (Bitboard bb) sq = testBit bb (fromEnum sq)

{-@ assume setSquare :: x:Bitboard -> Square -> {y:Bitboard | getPopulation y = getPopulation x + 1 || getPopulation y = getPopulation x} @-}
{-|
    Sets the square in the bitboard-}
setSquare :: Bitboard -> Square -> Bitboard
setSquare (Bitboard bb) sq = Bitboard $ setBit bb (fromEnum sq)

{-@ assume unsetSquare :: x:Bitboard -> Square -> {y:Bitboard | getPopulation y = getPopulation x - 1 || getPopulation y = getPopulation x} @-}
{-|
    Unsets the square in the bitboard-}
unsetSquare :: Bitboard -> Square -> Bitboard
unsetSquare (Bitboard bb) sq = Bitboard $ clearBit bb (fromEnum sq)

{-@ (<<>>) :: Bitboard -> Square -> Bitboard @-}
{-|
    Returns the union of the two bitboards-}
(<<>>) :: Bitboard -> Square -> Bitboard
bb <<>> i = bb <> setSquare mempty i


{-@ showBits :: Bitboard -> Text @-}
{-|
    Shows the bitboard in a square representation-}
showBits :: Bitboard -> Text
showBits (Bitboard bb) = showBits' 63
    where
        showBit :: Int -> Text
        showBit i = let
                        b = getSquare (Bitboard bb) (toEnum i)
                    in if b then "#" else "."
        line i = if i `mod` 8 == 0 then " " <> "" <> "\n" else ""
        showBits' i
            | i<0 || i>=64 = ""
            | i==0 = showBit i <> line i <> "abcdefgh\n"
            | otherwise = showBit i <> line i <> showBits' (i-1)