{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use camelCase" #-}

{-@ LIQUID "--reflection" @-}
{-@ LIQUID "--ple" @-}
{-@ LIQUID "--no-termination" @-}
-- {-@ LIQUID "--ple-with-undecided-guards" @-}
-- {-@ LIQUID "--extensionality" @-}
-- {-@ LIQUID "--counter-examples" @-}

{-|
Module      : Bits
Description : Module for bit manipulation functions
Copyright   : (c) 2023 Matteo Mariotti
License     : GNU GPL v.3
Maintainer  : matteomariotti0301@gmail.com
Stability   : experimental
Portability : POSIX

Wrappers around the standard bit functions to make them linear and add liquidhaskell annotations.
For now they are all specialized to 64 bit integers, because I'm waiting for liquidhaskell to support ghc 9.8.1 and
see if the extended type literals will allow me to use the `Word64` type.
-}
module Bits where

import Data.Bits
import Unsafe.Linear

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

{-@ reflect get_n_bit @-}
get_n_bit :: Int -> Int -> Bool
get_n_bit x n
    | n == 63 && x < 0 = True
    | x < 0 = not (get_n_bit (-x - 1) n)
    | otherwise =
        let
            {-@ assume p :: {x:Int | x /= 0} @-}
            p = mask_n_bit n
         in
            (x `div` p) `mod` 2 == 1

{-@ reflect clear_bit @-}
clear_bit :: Int -> Int -> Int
clear_bit x n =
    let
        p = mask_n_bit n
     in
        x - if get_n_bit x n then p else 0

{-@ reflect set_bit @-}
set_bit :: Int -> Int -> Int
set_bit x n =
    let
        p = mask_n_bit n
     in
        x + if get_n_bit x n then 0 else p

{-@ reflect pop @-}
pop :: Int -> Int
pop 0 = 0
pop x = if x < 0 then 1 + pop (x + mask_n_bit 63) else (x `mod` 2) + pop (x `div` 2)

{-@ x :: {a:Int | pop a <= 32} @-}
x :: Int
x = 0x0FFF00000000FFFF

{-@ reflect complement_helper @-}
complement_helper :: Int -> Int -> Int
complement_helper n x
    | n > 0 =
        if get_n_bit x n
            then complement_helper (n - 1) (clear_bit x (n - 1))
            else complement_helper (n - 1) (set_bit x (n - 1))
    | otherwise = x

{-@ reflect complement' @-}
complement' :: Int -> Int
complement' = complement_helper 64

{-@ reflect and_helper @-}
and_helper :: Int -> Int -> Int -> Int
and_helper n x y
    | n > 0 =
        if get_n_bit x n && get_n_bit y n
            then and_helper (n - 1) (set_bit x n) y
            else and_helper (n - 1) (clear_bit x n) y
    | otherwise = x

{-@ reflect and' @-}
and' :: Int -> Int -> Int
and' = and_helper 64

{-@ reflect or_helper @-}
or_helper :: Int -> Int -> Int -> Int
or_helper n x y
    | n > 0 =
        if get_n_bit x n || get_n_bit y n
            then or_helper (n - 1) (set_bit x n) y
            else or_helper (n - 1) (clear_bit x n) y
    | otherwise = x

{-@ reflect or' @-}
or' :: Int -> Int -> Int
or' = or_helper 64

{-@ reflect xor_helper @-}
xor_helper :: Int -> Int -> Int -> Int
xor_helper n x y
    | n > 0 =
        if get_n_bit x n /= get_n_bit y n
            then xor_helper (n - 1) (set_bit x n) y
            else xor_helper (n - 1) (clear_bit x n) y
    | otherwise = x

{-@ reflect xor' @-}
xor' :: Int -> Int -> Int
xor' = xor_helper 64

{-|
    Linear version of 'Data.Bits.popCount'.
-}

{-@ assume popCount :: x:Int -> {y:Pop | y = pop x} @-}
popCount :: Int %1 -> Int
popCount = toLinear Data.Bits.popCount

{-|
    Linear version of 'Data.Bits.testBit'.
-}

{-@ assume testBit :: n:Int -> i:Index -> {get_n_bit n i} @-}
testBit :: Int %1 -> Int %1 -> Bool
testBit = toLinear2 Data.Bits.testBit

{-@ assume $|$ :: x:Int -> y:Int -> {or' x y} @-}

{-|
    Linear version of 'Data.Bits..|.'.
-}
($|$) :: Int %1 -> Int %1 -> Int
($|$) = toLinear2 (Data.Bits..|.)

{-@ assume $&$ :: x:Int -> y:Int -> {and' x y} @-}

{-|
    Linear version of 'Data.Bits..&.'.
-}
($&$) :: Int %1 -> Int %1 -> Int
($&$) = toLinear2 (Data.Bits..&.)

{-@ assume xor :: x:Int -> y:Int -> {xor' x y} @-}

{-|
    Linear version of 'Data.Bits.xor'.
-}
xor :: Int %1 -> Int %1 -> Int
xor = toLinear2 Prelude.xor

{-|
    Linear version of 'Data.Bits.shiftL'.
-}
shiftL :: Int %1 -> Int %1 -> Int
shiftL = toLinear2 Data.Bits.shiftL

{-@ assume shiftR :: n:Int -> i:Int -> { div n (mask_n_bit i)} @-}

{-|
    Linear version of 'Data.Bits.shiftR'.
-}
shiftR :: Int %1 -> Int %1 -> Int
shiftR = toLinear2 Data.Bits.shiftR

{-|
    Linear version of 'Data.Bits.setBit'.
-}

{-@ assume setBit :: x:Int -> Index -> {y:Int | pop y = pop x || pop y = pop x + 1 } @-}
setBit :: Int %1 -> Int %1 -> Int
setBit = toLinear2 Data.Bits.setBit

{-|
    Linear version of 'Data.Bits.clearBit'.
-}

{-@ assume clearBit :: x:Int -> Index -> {y:Int | pop y = pop x || pop y = pop x - 1 } @-}
clearBit :: Int %1 -> Int %1 -> Int
clearBit = toLinear2 Data.Bits.clearBit

{-|
    Linear version of 'Data.Bits.complement'.
-}

{-@ assume complement :: x:Int -> { complement' x } @-}
complement :: Int %1 -> Int
complement = toLinear Data.Bits.complement

{-|
    Gets the last bit set to 1 or returns 0 instead.
-}
lastBit :: Int %1 -> Int
lastBit = toLinear (\x' -> x' Bits.$&$ Bits.complement x')
