{-|
Module      : Knights.Internal
Description : Knight representation using bitboards
Copyright   : (c) 2023-2024 Matteo Mariotti
License     : GNU GPL v.3
Stability   : experimental
Portability : POSIX

This module contains the representation of knights using bitboards, with also the relevant attack tables.
-}
{- FOURMOLU_DISABLE -}
{-|
-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}
{- FOURMOLU_ENABLE -}
module Knights.Internal (KnightBB (..), showAttacks, allocTable) where

import Bitboard
import Bits
import Common
import Data.Array.Destination
import Data.Vector
import Prelude.Linear (($))
import Prelude hiding (($))

{- {-@ type Pop = {x:Int | x >= 0 && x<= 64} @-}
{-@ type Index = {x:Int | x >= 0 && x<= 63} @-}
{-@ measure bbPop :: Bitboard -> Pop @-} -}
{-@ type KingBB = {x:Bitboard | bbPop x <= 10}  @-}

{-|
    The basic type for knight bitboards. It is a wrapper around 'Bitboard' that represents a bitboard with at most 10 bits set.
    The phantom type parameter specifies the side to move.
-}
newtype KnightBB = KnightBB Bitboard deriving (Eq, Show)

maskKnightAttack :: KnightBB -> AttackBB
maskKnightAttack (KnightBB b) =
    let
        initial :: Int = bb b
        not_a_file :: Int -> Int
        not_a_file res = res $&$ complement fileA
        not_h_file res = res $&$ complement fileH
        not_ab_file res = res $&$ complement fileA $&$ complement fileB
        not_gh_file res = res $&$ complement fileG $&$ complement fileH
        attacks =
            AttackBB
                $ not_a_file (initial `shiftL` 15)
                $|$ not_h_file (initial `shiftL` 17)
                $|$ not_ab_file (initial `shiftL` 6)
                $|$ not_gh_file (initial `shiftL` 10)
                $|$ not_h_file (initial `shiftR` 15)
                $|$ not_a_file (initial `shiftR` 17)
                $|$ not_gh_file (initial `shiftR` 6)
                $|$ not_ab_file (initial `shiftR` 10)
     in
        attacks

allocTable :: Vector AttackBB
allocTable = alloc 64 $ \newArr -> fromFunction fillFunction newArr
  where
    fillFunction :: Int -> AttackBB
    fillFunction i
        | i < 0 || i >= 64 = AttackBB 0
        | otherwise = maskKnightAttack (KnightBB (emptyBoard <<>> index2Square i))

showAttacks :: AttackBB -> Text
showAttacks (AttackBB bb) = showBits bb
