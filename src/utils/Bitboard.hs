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
import Prelude.Linear (($), (*))
import Unsafe.Linear
import Prelude hiding (($), (*))

-- import Data.Bits_LHAssumptions
import System.Random

{-@ LIQUID "--counter-examples" @-}

{-@ type Board = Word64 @-}

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
    deriving (Eq, Show)

emptyBoard :: Bitboard
emptyBoard = Bitboard 0

initialBoard :: Bitboard
initialBoard = Bitboard 0xFFFF00000000FFFF

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
    deriving (Eq, Ord, Show, Enum)

{-|
    Represents the file of a square on the board.
-}
data File = FA | FB | FC | FD | FE | FF | FG | FH deriving (Eq, Ord, Show, Enum)

{-|
    Represents the rank of a square on the board.
-}
data Rank = R1 | R2 | R3 | R4 | R5 | R6 | R7 | R8 deriving (Eq, Ord, Show, Enum)

{-|
    Returns the pieces on the given file of the board.
-}
maskFile :: File -> Word64 -> Word64
maskFile file bb = bb .&. (0x8080808080808080 `shiftR` fromEnum file)

{-|
    Converts a file to its corresponding bits on the board.
-}
file2Word :: File -> Word64
file2Word file = 0x8080808080808080 `shiftR` fromEnum file

{-|
    Returns the pieces on the given rank of the board.
-}
maskRank :: Rank -> Word64 %1 -> Word64
maskRank rank bb = bb .&. (0xFF `shiftL` (8 * toLinear fromEnum rank))

{-|
    Converts a rank to its corresponding bits on the board.
-}
rank2Word :: Rank %1 -> Word64
rank2Word rank = 0xFF `shiftL` (8 * toLinear fromEnum rank)

{-|
    Converts a square to its index in the bitboard.
-}

{-@ assume square2Index :: Square -> Index  @-}
square2Index :: Square %1 -> Int
square2Index = toLinear fromEnum

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
    line i = if i `mod` 8 == 0 then " " <> show (i `div` 8 + 1) <> "\n" else ""
    showBits' i
        | i < 0 || i >= 64 = ""
        | i == 0 =
            showBit i <> line i <> "a b c d e f g h\n" <> "Value: " <> show bb <> "\n"
        | otherwise = showBit i <> line i <> showBits' (i - 1)
