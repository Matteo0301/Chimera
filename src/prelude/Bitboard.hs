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
    --, showBits
    , getSquare
    , setSquare
    , (<<>>)
    , unsetSquare
    , getPopulation
) where

import Data.Bits ( Bits (..) )

{-@ LIQUID "--reflect" @-}
{-@ LIQUID "--ple" @-}


{-@ type Index = {v:Int | v >= 0 && v < 64} @-}
{-@ type Population = {v:Int | v >= 2 && v <= 32} @-}
{-@ type Board = {v:Word64 | (getPopulation v) >= 2 && (getPopulation v) <= 32} @-}


{-@ measure getPopulation :: Bitboard -> getPopulation @-}


{-@ data Bitboard = Bitboard (bb :: Word64) @-}
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

{-@ type Population = {v:Int | v >= 2 && v <= 32} @-}
{-@ type ValidBitboard = {v:Bitboard | (getPopulation (bb v)) >= 2 && (getPopulation (bb v)) <= 32} @-}

instance Semigroup Bitboard where
    (<>) :: Bitboard -> Bitboard -> Bitboard
    (Bitboard bb1) <> (Bitboard bb2) = Bitboard $ bb1 .|. bb2

instance Monoid Bitboard where
    mempty :: Bitboard
    mempty = Bitboard 0
    mappend :: Bitboard -> Bitboard -> Bitboard
    mappend  = (<>)


{- {-@ showBits :: Bitboard -> Text @-}
{-|
    Shows the bitboard in a square representation-}
showBits :: Bitboard -> Text
showBits (Bitboard bb) = showBits' bb 63 ""
  where
    {-@ assume showBits' :: Word64 -> Int -> Text -> Text @-}
    showBits' :: Word64 -> Int -> Text -> Text
    showBits' bb' i s
        | i == 0 = s
        | i `mod` 8 == 0 && i >= 0 && i< 64 = showBits' bb' (i - 1) (s <> (if testBit bb' i then "#\n" else ".\n"))
        | otherwise = showBits' (bb') (i - 1) (s <> (if testBit bb' i then "#" else ".")) -}


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

{-|
    Checks if the square at the given index is occupied-}
getSquare :: Bitboard -> Square -> Bool
getSquare (Bitboard bb) = testBit bb . fromEnum

{-|
    Sets the square at the given index as occupied-}
setSquare :: Bitboard -> Square -> Bitboard
setSquare (Bitboard bb) i = Bitboard  bb <> (Bitboard(1 `shiftL` fromEnum i))

{-|
    Operator version of 'setSquare'
-}
(<<>>) :: Bitboard -> Square -> Bitboard
(<<>>) = setSquare

{-|
    Sets the square at the given index as unoccupied-}
unsetSquare :: Bitboard -> Square -> Bitboard
unsetSquare (Bitboard bb) i = Bitboard $ clearBit bb (fromEnum i)


{-@ assume getPopulation :: Bitboard -> Population @-}
{-|
    Gets the number of squares occupied in the bitboard-}
getPopulation :: Bitboard -> Int
getPopulation (Bitboard bb) = popCount bb