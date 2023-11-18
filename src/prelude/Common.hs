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
    , file2Word
    , maskRank
    , rank2Word
    , square2Index
    , Piece (..)
    ) where

import Bits
import Unsafe.Linear

{-@ type Board = Word64 @-}

{-@ type Pop = {x:Int | x >= 0 && x<= 64} @-}
{-@ type Index = {x:Int | x >= 0 && x<= 63} @-}

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
rank2Word :: Rank -> Word64
rank2Word rank = 0xFF `shiftL` (8 * toLinear fromEnum rank)

{-|
    Converts a square to its index in the bitboard.
-}

{-@ assume square2Index :: Square -> Index  @-}
square2Index :: Square %1 -> Int
square2Index = toLinear fromEnum

{-|
    Represents the side to move.
-}
data SideToMove = White | Black

{-|
    Represents the squares attacked by a piece
-}
newtype AttackBB = AttackBB Word64 deriving (Eq, Show)

{-|
    Class to extract the value of the side to move from the corresponding type.
-}
class GetSide (a :: SideToMove) where
    getSide :: Proxy a -> SideToMove

instance GetSide 'White where
    getSide _ = White

instance GetSide 'Black where
    getSide _ = Black

{-|
    Class to extract the attacks of a piece from the corresponding type.
-}
class Piece a where
    getAttacks :: Proxy a -> Square -> AttackBB
