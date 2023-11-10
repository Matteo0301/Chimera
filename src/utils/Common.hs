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
    ) where

import Bitboard

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
