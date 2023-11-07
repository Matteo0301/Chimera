module Bits where

import Data.Bits
import Prelude.Linear
import Unsafe.Linear

{-@ embed Int * as int @-}
{-@ measure pop :: a -> Pop @-}

{-@ type Pop = {x:Int | x >= 0 && x<= 64} @-}
{-@ type Index = {x:Int | x >= 0 && x<= 63} @-}

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
xor = toLinear2 Data.Bits.xor

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