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
    , PieceBB (..)
    , AttackBB (..)
    ) where

import Bitboard

{-|
    Represents the side to move.
-}
data SideToMove = White | Black

-- \|
--    Class that represent a bitboard for a specific type of piece.
class PieceBB a where
    -- |
    --         Returns the squares attacked by the given piece on the board.
    getAttacks :: Bitboard -> a -> AttackBB

    -- |
    --         Returns the side to move.
    side :: Proxy a -> SideToMove

{-|
    Represents the squares attacked by a piece
-}
newtype AttackBB = AttackBB Word64