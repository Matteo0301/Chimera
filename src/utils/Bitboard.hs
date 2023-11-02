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
    , trySet
    , showBits
    , getSquare
    , setSquare
    , (<<>>)
    , unsetSquare
    , getPopulation
    , emptyBoard
    ) where

import Control.Exception
import Data.Bits (Bits (..))

{-@ LIQUID "--counter-examples" @-}
{-@ LIQUID "--diff" @-}

{-@ type Board = Word64 @-}

{-@ using (Bitboard) as {x:Bitboard| getPopulation x <= 32} @-}

{-|
    The 'Bitboard' type is a newtype wrapper around 'Word64' that represents a bitboard.
    The bitboard is a 64-bit integer where each bit represents a square on the board.
    The least significant bit represents the square h1, the most significant bit represents the square a8.
    The bitboard is stored in little endian order, so the first 8 bits represent the first row of the board.
-}
newtype Bitboard = Bitboard Word64
    deriving (Eq, Show)

emptyBoard :: Bitboard
emptyBoard = Bitboard 0

{-|
    The 'Square' type represents the index of a square on the board.
-}
data Square
    = H1
    | G1
    | F1
    | E1
    | D1
    | C1
    | B1
    | A1
    | H2
    | G2
    | F2
    | E2
    | D2
    | C2
    | B2
    | A2
    | H3
    | G3
    | F3
    | E3
    | D3
    | C3
    | B3
    | A3
    | H4
    | G4
    | F4
    | E4
    | D4
    | C4
    | B4
    | A4
    | H5
    | G5
    | F5
    | E5
    | D5
    | C5
    | B5
    | A5
    | H6
    | G6
    | F6
    | E6
    | D6
    | C6
    | B6
    | A6
    | H7
    | G7
    | F7
    | E7
    | D7
    | C7
    | B7
    | A7
    | H8
    | G8
    | F8
    | E8
    | D8
    | C8
    | B8
    | A8
    deriving (Eq, Show, Enum)

{-@ assume getPopulation :: Bitboard -> {x:Int | x>=0 || x<=64} @-}
{-@ measure getPopulation :: Bitboard -> Int @-}
getPopulation :: Bitboard -> Int
getPopulation (Bitboard bb) = popCount bb

{-@ getSquare :: Bitboard -> Square -> Bool @-}

{-|
    Returns whether the square is set in the bitboard
-}
getSquare :: Bitboard -> Square -> Bool
getSquare (Bitboard bb) sq = testBit bb (fromEnum sq)

{-@ assume setSquare :: x:Bitboard -> Square -> {y:Bitboard | if getPopulation x == 32 then getPopulation y = 32 else getPopulation y = getPopulation x + 1} @-}

{-|
    Sets the square in the bitboard. If the new bitboard has more that 32 squares occupied, returns the old one.
-}
setSquare :: Bitboard -> Square -> Bitboard
setSquare (Bitboard bb) sq =
    let
        new = Bitboard $ setBit bb (fromEnum sq)
     in
        assert (getPopulation new <= 32) new

{-@ assume unsetSquare :: x:Bitboard -> Square -> {y:Bitboard | if getPopulation x == 0 then getPopulation y = 0 else getPopulation y = getPopulation x - 1} @-}

{-|
    Sets a certain square in the board as empty
-}
unsetSquare :: Bitboard -> Square -> Bitboard
unsetSquare (Bitboard bb) sq = Bitboard $ clearBit bb (fromEnum sq)

{-@ (<<>>) :: Bitboard -> Square -> Bitboard @-}

{-|
    The operator version of 'setSquare'
-}
(<<>>) :: Bitboard -> Square -> Bitboard
bb <<>> i = setSquare bb i

{-@ assume trySet :: Word64 -> Bitboard @-}

{-|
    Returns the bitboard if the population is between 0 and 32, otherwise returns the empty bitboard. This should only be used in tests.
-}
trySet :: Word64 -> Bitboard
trySet x =
    let
        bb = Bitboard x
        pop = getPopulation bb
     in
        if pop <= 32 then bb else Bitboard 1

{-@ showBits :: Bitboard -> Text @-}

{-|
    Shows the bitboard in a square representation, along with its numeric value
-}
showBits :: Bitboard -> Text
showBits (Bitboard bb) = showBits' 63
  where
    showBit :: Int -> Text
    showBit i =
        let
            b = getSquare (Bitboard bb) (toEnum i)
         in
            if b then "# " else ". "
    line i = if i `mod` 8 == 0 then " " <> show (i `div` 8 + 1) <> "\n" else ""
    showBits' i
        | i < 0 || i >= 64 = ""
        | i == 0 =
            showBit i <> line i <> "a b c d e f g h\n" <> "Value: " <> show bb <> "\n"
        | otherwise = showBit i <> line i <> showBits' (i - 1)
