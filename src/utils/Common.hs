{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}

module Common
    ( SideToMove (..)
    , HasSide (..)
    , Attack (..)
    , AttackBB (..)
    ) where

import Bitboard

{-|
    Represents the side to move.
-}
data SideToMove = White | Black

{-|
    Class for types that represent the side to move.
-}
class HasSide a where
    -- |
    --         Returns the side to move.
    getSide :: a -> SideToMove

-- \|
--    Class for types that represent pieces on the board to calculate the attack squares.
class Attack a where
    -- |
    --         Returns the squares attacked by the given piece on the board.
    getAttacks :: Bitboard -> a -> SideToMove -> AttackBB

{-|
    Represents the squares attacked by a piece
-}
newtype AttackBB = AttackBB Word64