{-|
Module      : Bits
Description : Module for bit manipulation functions
Copyright   : (c) 2023-2024 Matteo Mariotti
License     : GNU GPL v.3
Stability   : experimental
Portability : POSIX

Wrappers around the standard bit functions to make them linear and add liquidhaskell annotations.
For now they are all specialized to 64 bit integers, because I'm waiting for liquidhaskell to support ghc 9.8.1 and
see if the extended type literals will allow me to use the `Word64` type.
-}
{- FOURMOLU_DISABLE -}
{-|
-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# LANGUAGE NoImplicitPrelude #-}

{-# HLINT ignore "Use camelCase" #-}
{- FOURMOLU_ENABLE -}
module Bits where

import Prelude hiding ((&&&))
import Data.Bits
import GHC.Base (Int (I#), iShiftRL#)
import Language.Haskell.Liquid.ProofCombinators

{-@ LIQUID "--reflection" @-}
{-@ LIQUID "--ple" @-}
{-@ LIQUID "--no-termination" @-}
{-@ LIQUID "--ple-with-undecided-guards" @-}
{-@ LIQUID "--extensionality" @-}
--{-@ LIQUID "--higherorder" @-}
-- {-@ LIQUID "--counter-examples" @-}

{-@ embed Int * as int @-}

{-@ type Pop = {x:Int | x >= 0 && x<= 64} @-}
{-@ type Index = {x:Int | x >= 0 && x<= 63} @-}

-- helper functions to be used in refinement types
{-@ reflect mask_n_bit @-}
mask_n_bit :: Int -> Int
mask_n_bit 0 = 1
mask_n_bit n
    | n > 0 = 2 * mask_n_bit (n - 1)
    | otherwise = 1

{-@ reflect div' @-}
div' :: Int -> Int -> Int
div' _ 0 = 0
div' x y
    | x < 0 = -div' (-x) y
    | y < 0 = -div' x (-y)
    | x < y = 0
    | otherwise = 1 + div' (x - y) y

{-@ reflect get_n_bit @-}
get_n_bit :: Int -> Int -> Bool
get_n_bit x n
    | n == 63 = x < 0
    | n < 0 = False
    | x < 0 = not (get_n_bit (-x - 1) n)
    | otherwise =
        let
            {-@ assume p :: {x:Int | x /= 0} @-}
            p = mask_n_bit n
         in
            (x `div'` p) `mod` 2 == 1

{-@ reflect clear_bit @-}
clear_bit :: Int -> Int -> Int
clear_bit x n =
    let
        p = mask_n_bit n
     in
        x - if get_n_bit x n then p else 0

{-@ reflect set_bit @-}
set_bit :: Int -> Int -> Int
set_bit x n = pleUnfold (x + if get_n_bit x n then 0 else mask_n_bit n)

{-@ reflect pop @-}
pop :: Int -> Int
pop 0 = 0
pop x = if x < 0 then 1 + pop (x + mask_n_bit 63) else x `mod` 2 + pop (x `div` 2)

{-@ reflect complement_helper @-}
complement_helper :: Int -> Int -> Int
complement_helper n x
    | n >= 0 =
        if get_n_bit x n
            then complement_helper (n - 1) (clear_bit x n)
            else complement_helper (n - 1) (set_bit x n)
    | otherwise = x

{-@ reflect complement' @-}
complement' :: Int -> Int
complement' = complement_helper 63

{-@ reflect and_helper @-}
and_helper :: Int -> Int -> Int -> Int
and_helper n x y
    | n >= 0 =
        if get_n_bit x n && get_n_bit y n
            then and_helper (n - 1) (set_bit x n) y
            else and_helper (n - 1) (clear_bit x n) y
    | otherwise = x

{-@ reflect and' @-}
and' :: Int -> Int -> Int
and' = and_helper 63

{-@ reflect or_helper @-}
or_helper :: Int -> Int -> Int -> Int
or_helper n x y
    | n >= 0 =
        if get_n_bit x n || get_n_bit y n
            then or_helper (n - 1) (set_bit x n) y
            else or_helper (n - 1) (clear_bit x n) y
    | otherwise = x

{-@ reflect or' @-}
or' :: Int -> Int -> Int
or' = or_helper 63

{-@ reflect xor_helper @-}
xor_helper :: Int -> Int -> Int -> Int
xor_helper n x y
    | n >= 0 =
        if get_n_bit x n /= get_n_bit y n
            then xor_helper (n - 1) (set_bit x n) y
            else xor_helper (n - 1) (clear_bit x n) y
    | otherwise = x

{-@ reflect xor' @-}
xor' :: Int -> Int -> Int
xor' = xor_helper 63

{-@ reflect shiftR_helper @-}
shiftR_helper :: Int -> Int -> Int -> Int
shiftR_helper n x i
    | n >= 64 - i = shiftR_helper (63 - i) x i
    | n >= 0 =
        let res = shiftR_helper (n - 1) x i
         in if get_n_bit x (n + i)
                then set_bit res n
                else res
    | otherwise = 0

{-@ reflect shiftR' @-}
shiftR' :: Int -> Int -> Int
shiftR' = shiftR_helper 63

{-@ reflect shiftL_helper @-}
shiftL_helper :: Int -> Int -> Int -> Int
shiftL_helper n x i
    | n >= 64 = shiftL_helper 63 x i
    | n >= 0 =
        let res = shiftL_helper (n - 1) x i
         in if get_n_bit x (n - i)
                then set_bit res n
                else res
    | otherwise = 0

{-@ reflect shiftL' @-}
shiftL' :: Int -> Int -> Int
shiftL' = shiftL_helper 63

{-|
    Version of 'Data.Bits.popCount' with refinement types.
-}

{-@ assume popCount :: x:Int -> {y:Pop | y = pop x} @-}
popCount :: Int -> Int
popCount = Data.Bits.popCount

{-|
    Version of 'Data.Bits.testBit' with refinement types.
-}

{-@ assume testBit :: n:Int -> i:Index -> {get_n_bit n i} @-}
testBit :: Int -> Int -> Bool
testBit = Data.Bits.testBit

{-@ assume $|$ :: x:Int -> y:Int -> {or' x y} @-}

{-|
    Version of 'Data.Bits..|.' with refinement types.
-}
($|$) :: Int -> Int -> Int
($|$) = (Data.Bits..|.)

infixl 5 $|$

{-@ assume $&$ :: x:Int -> y:Int -> {and' x y} @-}

{-|
    Version of 'Data.Bits..&.' with refinement types.
-}
($&$) :: Int -> Int -> Int
($&$) = (Data.Bits..&.)

infixl 7 $&$

{-@ assume xor :: x:Int -> y:Int -> {xor' x y} @-}

{-|
    Version of 'Data.Bits.xor' with refinement types.
-}
xor :: Int -> Int -> Int
xor = Prelude.xor

infixl 6 `xor`

{-@ assume shiftL :: n:Int -> i:Int -> { shiftL' n i} @-}

{-|
    Version of 'Data.Bits.shiftL' with refinement types. The first argument is the number to shift, the second is the number of bits to shift.
-}
shiftL :: Int -> Int -> Int
shiftL = Data.Bits.shiftL

{-@ assume shiftR :: n:Int -> i:Int -> { shiftR' n i} @-}

infixl 8 `shiftL`

{-|
    A Version of logic (unsigned) right shift with refinement types. The first argument is the number to shift, the second is the number of bits to shift.
-}
shiftR :: Int -> Int -> Int
shiftR =
    liftShiftR
  where
    liftShiftR :: Int -> Int -> Int
    liftShiftR (I# x) (I# i) = I# (x `iShiftRL#` i)

infixl 8 `shiftR`

{-|
    Version of 'Data.Bits.setBit' with refinement types.
-}

{-@ assume setBit :: x:Int -> Index -> {y:Int | pop y = pop x || pop y = pop x + 1 } @-}
setBit :: Int -> Int -> Int
setBit = Data.Bits.setBit

{-|
    Version of 'Data.Bits.clearBit' with refinement types.
-}

{-@ assume clearBit :: x:Int -> Index -> {y:Int | pop y = pop x || pop y = pop x - 1 } @-}
clearBit :: Int -> Int -> Int
clearBit = Data.Bits.clearBit

{-|
    Version of 'Data.Bits.complement' with refinement types.
-}

{-@ assume complement :: x:Int -> { complement' x } @-}
complement :: Int -> Int
complement = Data.Bits.complement

{-|
    Gets the last bit set to 1 or returns 0 instead.
-}
lastBit :: Int -> Int
lastBit x' = x' Bits.$&$ Bits.complement x'

{-|
    Version of 'Data.Bits.countLeadingZeros' with refinement types.
-}
countTrailingZeros :: Int -> Int
countTrailingZeros = Data.Bits.countTrailingZeros
