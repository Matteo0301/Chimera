{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# OPTIONS_GHC -Wno-overflowed-literals #-}

{-|
Module      : Common
Description : Common utils
Copyright   : (c) 2023 Matteo Mariotti
License     : GNU GPL v.3
Maintainer  : matteomariotti0301@gmail.com
Stability   : experimental
Portability : POSIX

A collection of common utilities used in the project.
-}
module Common
    ( SideToMove (..)
    , AttackBB (..)
    , GetSide (..)
    , Square (..)
    , File (..)
    , Rank (..)
    , maskFile
    , file2Int
    , maskRank
    , rank2Int
    , square2Index
    , Piece (..)
    , squareMask
    , attacks2Int
    , showBits
    , printAttacks
    ) where

import Bits
import Unsafe.Linear

{-@ type Board = Int @-}

{-@ type Pop = {x:Int | x >= 0 && x<= 64} @-}
{-@ type Index = {x:Int | x >= 0 && x<= 63} @-}

{-|
    The 'Square' type represents the index of a square on the board.
-}
data Square
    = A8
    | B8
    | C8
    | D8
    | E8
    | F8
    | G8
    | H8
    | A7
    | B7
    | C7
    | D7
    | E7
    | F7
    | G7
    | H7
    | A6
    | B6
    | C6
    | D6
    | E6
    | F6
    | G6
    | H6
    | A5
    | B5
    | C5
    | D5
    | E5
    | F5
    | G5
    | H5
    | A4
    | B4
    | C4
    | D4
    | E4
    | F4
    | G4
    | H4
    | A3
    | B3
    | C3
    | D3
    | E3
    | F3
    | G3
    | H3
    | A2
    | B2
    | C2
    | D2
    | E2
    | F2
    | G2
    | H2
    | A1
    | B1
    | C1
    | D1
    | E1
    | F1
    | G1
    | H1
    deriving (Eq, Ord, Show, Enum)

{-@ data File = FA | FB | FC | FD | FE | FF | FG | FH @-}

{-|
    Represents the file of a square on the board.
-}
data File = FA | FB | FC | FD | FE | FF | FG | FH deriving (Eq, Ord, Show, Enum)

{-@ data Rank = R1 | R2 | R3 | R4 | R5 | R6 | R7 | R8 @-}

{-|
    Represents the rank of a square on the board.
-}
data Rank = R1 | R2 | R3 | R4 | R5 | R6 | R7 | R8 deriving (Eq, Ord, Show, Enum)

{-@ maskFile :: File -> Int -> Int @-}

{-|
    Returns the pieces on the given file of the board.
-}
maskFile :: File -> Int -> Int
maskFile file bb = bb $&$ file2Int file

{-@ file2Int :: File -> Int @-}

{-|
    Converts a file to its corresponding bits on the board.
-}
file2Int :: File -> Int
file2Int file = 0x8080808080808080 `shiftR` fromEnum file

{-@ maskRank :: Rank -> Int -> Int @-}

{-|
    Returns the pieces on the given rank of the board.
-}
maskRank :: Rank -> Int %1 -> Int
maskRank rank bb = bb $&$ rank2Int rank

{-@ rank2Int :: Rank -> Int @-}

{-|
    Converts a rank to its corresponding bits on the board.
-}
rank2Int :: Rank -> Int
rank2Int rank = 0xFF `shiftL` (8 * toLinear fromEnum rank)

{-|
    Converts a square to its index in the bitboard.
-}

{-@ assume square2Index :: Square -> Index  @-}
square2Index :: Square %1 -> Int
square2Index = toLinear fromEnum

squareMask :: Square -> Int
squareMask s = 1 `shiftL` square2Index s

{-|
    Represents the side to move.
-}
data SideToMove = White | Black

{-|
    Represents the squares attacked by a piece
-}
newtype AttackBB = AttackBB Int deriving (Eq, Show)

attacks2Int :: AttackBB -> Int
attacks2Int (AttackBB bb) = bb

{-|
    Class to extract the value of the side to move from the corresponding type.
-}
class GetSide (a :: SideToMove) where
    getSide :: Proxy a -> SideToMove

instance GetSide 'White where
    getSide :: Proxy 'White -> SideToMove
    getSide _ = White

instance GetSide 'Black where
    getSide :: Proxy 'Black -> SideToMove
    getSide _ = Black

{-|
    Class to extract the attacks of a piece from the corresponding type.
-}
class Piece a where
    getAttacks :: Square -> AttackBB

printAttacks :: forall a. (Piece a) => Square -> Text
printAttacks s = showBits $ attacks2Int $ getAttacks @a s

{-@ ignore showBits @-}

{-|
    Shows the bitboard in a square representation, along with its numeric value
-}
showBits :: Int -> Text
showBits bb = showBits' 0
  where
    showBit :: Int -> Text
    showBit i =
        let
            b = testBit bb i
         in
            if b then "# " else ". "
    line :: Int -> Text
    line i = if i `mod` 8 == 7 then " " <> show (8 - (i `div` 8)) <> "\n" else ""
    showBits' i
        | i < 0 || i >= 64 = ""
        | i == 63 =
            showBit i <> line i <> "a b c d e f g h\n" <> "Value: " <> show bb <> "\n"
        | otherwise = showBit i <> line i <> showBits' (i + 1)