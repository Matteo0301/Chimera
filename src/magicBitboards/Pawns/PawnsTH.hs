{-# LANGUAGE TemplateHaskell #-}

module Pawns.PawnsTH where

import Bitboard
import Common
import Common (GetSide)
import Data.Vector
import Pawns.Pawns

tableWhite :: Vector AttackBB
tableWhite = $([|allocTable @'White|])

tableBlack :: Vector AttackBB
tableBlack = $([|allocTable @'Black|])

instance (GetSide a) => Piece (PawnBB a) where
    {-# INLINE getAttacks #-}
    {-# SPECIALIZE getAttacks :: Proxy (PawnBB 'White) -> Square -> AttackBB #-}
    {-# SPECIALIZE getAttacks :: Proxy (PawnBB 'Black) -> Square -> AttackBB #-}
    getAttacks _ s = case getSide (Proxy :: Proxy a) of
        White -> tableWhite ! square2Index s
        Black -> tableBlack ! square2Index s