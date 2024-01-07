{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}

{-|
Module      : Knights.Internal
Description : Knight representation using bitboards
Copyright   : (c) 2023 Matteo Mariotti
License     : GNU GPL v.3
Maintainer  : matteomariotti0301@gmail.com
Stability   : experimental
Portability : POSIX

This module contains the representation of knights using bitboards, with also the relevant attack tables.
-}
module Knights.Internal (KnightBBWrapped (..), showAttacks, allocTable) where

import Bitboard
import Bits
import Common
import Data.Array.Destination
import Data.Vector
import Prelude.Linear (($))
import Util
import Prelude hiding (($))

-- {-@ LIQUID "--no-termination" @-}

{- {-@ type Pop = {x:Int | x >= 0 && x<= 64} @-}
{-@ type Index = {x:Int | x >= 0 && x<= 63} @-}
{-@ measure bbPop :: Bitboard -> Pop @-} -}
-- {-@ data KnightBBWrapped = KnightBBWrapped (bb :: {x:Bitboard | bbPop x <= 10}) @-}

newtype KnightBBWrapped = KnightBBWrapped Bitboard deriving (Eq, Show)

maskKnightAttack :: KnightBBWrapped -> AttackBB
maskKnightAttack (KnightBBWrapped bb) =
    let
        initial :: Int = bb2Int bb
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
        | otherwise = maskKnightAttack (KnightBBWrapped (emptyBoard <<>> toEnum i))

showAttacks :: AttackBB -> Text
showAttacks (AttackBB bb) = showBits bb
