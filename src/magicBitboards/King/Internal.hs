{-|
Module      : King.Internal
Description : King representation using bitboards
Copyright   : (c) 2023-2024 Matteo Mariotti
License     : GNU GPL v.3
Stability   : experimental
Portability : POSIX

This module contains the representation of kings using bitboards, with also the relevant attack tables.
-}
{- FOURMOLU_DISABLE -}
{-|
-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}
{- FOURMOLU_ENABLE -}
module King.Internal (KingBB (..), showAttacks, allocTable) where

import Bitboard
import Bits
import Common
import Data.Array.Destination
import Data.Vector
import Prelude.Linear (($))
import Prelude hiding (xor, ($))

{- {-@ type Pop = {x:Int | x >= 0 && x<= 64} @-}
{-@ type Index = {x:Int | x >= 0 && x<= 63} @-}
{-@ measure bbPop :: Bitboard -> Pop @-} -}
{-@ type KingBB = {x:Bitboard | bbPop x == 2}  @-}

{-|
    The basic type for knight bitboards. It is a wrapper around 'Bitboard' that represents a bitboard with at most 10 bits set.
    The phantom type parameter specifies the side to move.
-}
newtype KingBB = KingBB Bitboard deriving (Eq, Show)

maskKingAttack :: KingBB -> AttackBB
maskKingAttack (KingBB b) =
    let
        initial :: Int = bb b
        not_a_file res = res $&$ complement fileA
        not_h_file res = res $&$ complement fileH
        tmp =
            not_h_file (initial `shiftR` 1) $|$ not_a_file (initial `shiftL` 1) $|$ initial
        attacks = ((tmp `shiftR` 8) $|$ tmp $|$ (tmp `shiftL` 8)) `xor` initial
     in
        AttackBB attacks

allocTable :: Vector AttackBB
allocTable = alloc 64 $ \newArr -> fromFunction fillFunction newArr
  where
    fillFunction :: Int -> AttackBB
    fillFunction i
        | i < 0 || i >= 64 = AttackBB 0
        | otherwise = maskKingAttack (KingBB (emptyBoard <<>> index2Square i))

showAttacks :: AttackBB -> Text
showAttacks (AttackBB bb) = showBits bb
