{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use camelCase" #-}

{-@ LIQUID "--reflection" @-}
{-@ LIQUID "--ple" @-}
-- {-@ LIQUID "--ple-with-undecided-guards" @-}
-- {-@ LIQUID "--extensionality" @-}
{-@ LIQUID "--counter-examples" @-}

{-|
Module      : Bits
Description : Module for bit manipulation functions
Copyright   : (c) 2023 Matteo Mariotti
License     : GNU GPL v.3
Maintainer  : matteomariotti0301@gmail.com
Stability   : experimental
Portability : POSIX

Wrappers around the standard bit functions to make them linear and add liquidhaskell annotations.
-}
module Bits where

import Data.Bits
import Prelude.Linear (Movable)
import Unsafe.Linear

{-@ embed Int * as int @-}


{-@ type Pop = {x:Int | x >= 0 && x<= 64} @-}
{-@ type Index = {x:Int | x >= 0 && x<= 63} @-}

-- helper function to get an integer with the n-th bit set to 1
{-@ reflect mask_n_bit @-}
mask_n_bit :: Int -> Int
mask_n_bit 0 = 1
mask_n_bit n
    | n > 0 = 2 * mask_n_bit (n - 1)
    | otherwise = 1

{-@ reflect get_n_bit @-}
get_n_bit :: Int -> Int -> Bool
get_n_bit x n =
    let
        {-@ assume p :: {x:Int | x /= 0} @-}
        p = mask_n_bit n
     in
        (x `div` p) `mod` 2 == 1

{-@ reflect clear_bit @-}
-- {-@ clear_bit :: Int -> i:Index -> {x:Int | not (get_n_bit x i) } @-}
clear_bit :: Int -> Int -> Int
clear_bit x n =
    let
        p = mask_n_bit n
     in
        x - if get_n_bit x n then p else 0

{-@ reflect set_bit @-}
-- {-@ set_bit :: Int -> i:Index -> {x:Int | (get_n_bit x i) } @-}
set_bit :: Int -> Int -> Int
set_bit x n =
    let
        p = mask_n_bit n
     in
        x + if get_n_bit x n then 0 else p

{-@ reflect get_pop @-}
get_pop :: Int -> Int
get_pop 0 = 0
get_pop x = (x `mod` 2) + get_pop (x `div` 2)

{-@ reflect pop @-}
pop :: (Integral a) => a -> Int
pop x = get_pop  (fromIntegral  x)


{-|
    Linear version of 'Data.Bits.popCount'.
-}

{-@ assume popCount :: x:a -> {y:Pop | y = pop x} @-}
popCount :: (FiniteBits a, Movable a) => a %1 -> Int
popCount = toLinear Data.Bits.popCount

{-|
    Linear version of 'Data.Bits.testBit'.
-}

{-@ testBit :: a -> Index -> Bool @-}
testBit :: (FiniteBits a, Movable a) => a %1 -> Int %1 -> Bool
testBit = toLinear2 Data.Bits.testBit

{-|
    Linear version of 'Data.Bits..|.'.
-}
(.|.) :: (FiniteBits a, Movable a) => a %1 -> a %1 -> a
(.|.) = toLinear2 (Data.Bits..|.)

{-|
    Linear version of 'Data.Bits..&.'.
-}
(.&.) :: (FiniteBits a, Movable a) => a %1 -> a %1 -> a
(.&.) = toLinear2 (Data.Bits..&.)

{-|
    Linear version of 'Data.Bits.xor'.
-}
xor :: (FiniteBits a, Movable a) => a %1 -> a %1 -> a
xor = toLinear2 Prelude.xor

{-|
    Linear version of 'Data.Bits.shiftL'.
-}
shiftL :: (FiniteBits a, Movable a) => a %1 -> Int %1 -> a
shiftL = toLinear2 Data.Bits.shiftL

{-|
    Linear version of 'Data.Bits.shiftR'.
-}
shiftR :: (FiniteBits a, Movable a) => a %1 -> Int %1 -> a
shiftR = toLinear2 Data.Bits.shiftR

{-|
    Linear version of 'Data.Bits.setBit'.
-}

{-@ assume setBit :: x:a -> Index -> {y:a | pop y = pop x || pop y = pop x + 1 } @-}
setBit :: (FiniteBits a, Movable a) => a %1 -> Int %1 -> a
setBit = toLinear2 Data.Bits.setBit

{-|
    Linear version of 'Data.Bits.clearBit'.
-}

{-@ assume clearBit :: x:a -> Index -> {y:a | pop y = pop x || pop y = pop x - 1 } @-}
clearBit :: (FiniteBits a, Movable a) => a %1 -> Int %1 -> a
clearBit = toLinear2 Data.Bits.clearBit

{-|
    Linear version of 'Data.Bits.complement'.
-}

{-@ assume complement :: x:a -> {y:a | pop y = 64 - pop x } @-}
complement :: (FiniteBits a, Movable a) => a %1 -> a
complement = toLinear Data.Bits.complement

{-|
    Gets the last bit set to 1 or returns 0 instead.
-}
lastBit :: (FiniteBits a, Movable a) => a -> a
lastBit x = x Bits..&. Bits.complement x
