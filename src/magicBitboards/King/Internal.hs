{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}

{-|
Module      : King.Internal
Description : King representation using bitboards
Copyright   : (c) 2023 Matteo Mariotti
License     : GNU GPL v.3
Maintainer  : matteomariotti0301@gmail.com
Stability   : experimental
Portability : POSIX

This module contains the representation of knights using bitboards, with also the relevant attack tables.
-}
module King.Internal (KingBBWrapped (..), showAttacks, allocTable) where

import Bitboard
import Bits
import Common
import Data.Array.Destination
import Data.Vector
import Prelude.Linear (($))
import Prelude hiding (xor, ($))

{-@ LIQUID "--no-termination" @-}

{- {-@ type Pop = {x:Int | x >= 0 && x<= 64} @-}
{-@ type Index = {x:Int | x >= 0 && x<= 63} @-}
{-@ measure bbPop :: Bitboard -> Pop @-} -}
{-@ data KingBBWrapped = KingBBWrapped (bb :: {x:Bitboard | bbPop x == 1}) @-}

newtype KingBBWrapped = KingBBWrapped Bitboard deriving (Eq, Show)

maskKingAttack :: KingBBWrapped -> AttackBB
maskKingAttack (KingBBWrapped bb) =
    let
        initial :: Word64 = bb2Word bb
        not_a_file :: Word64 -> Word64
        not_a_file res = res .&. complement (file2Word FA)
        not_h_file res = res .&. complement (file2Word FH)
        tmp =
            not_a_file (initial `shiftR` 1) .|. not_h_file (initial `shiftL` 1) .|. initial
        attacks = (tmp `shiftR` 8) .|. tmp .|. (tmp `shiftL` 8) `xor` initial
     in
        AttackBB attacks

allocTable :: Vector AttackBB
allocTable = alloc 64 $ \newArr -> fromFunction fillFunction newArr
  where
    fillFunction :: Int -> AttackBB
    fillFunction i
        | i < 0 || i >= 64 = AttackBB 0
        | otherwise = maskKingAttack (KingBBWrapped (emptyBoard <<>> toEnum i))

showAttacks :: AttackBB -> Text
showAttacks (AttackBB bb) = showBits bb
