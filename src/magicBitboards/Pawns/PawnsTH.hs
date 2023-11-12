{-# LANGUAGE TemplateHaskell #-}

module Pawns.PawnsTH where

import Bitboard
import Common
import Data.Vector
import Language.Haskell.TH
import Pawns.Pawns

tableWhite :: Vector AttackBB
tableWhite = $([|allocTable @'White|])

tableBlack :: Vector AttackBB
tableBlack = $([|allocTable @'Black|])

{-# INLINE getAttacks #-}
{-# SPECIALIZE getAttacks :: Proxy 'White -> Square -> AttackBB #-}
{-# SPECIALIZE getAttacks :: Proxy 'Black -> Square -> AttackBB #-}
getAttacks :: forall a. (GetSide a) => Proxy a -> Square -> AttackBB
getAttacks _ s = case getSide (Proxy :: Proxy a) of
    White -> tableWhite ! square2Index s
    Black -> tableBlack ! square2Index s