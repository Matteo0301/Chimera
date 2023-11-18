{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE NoImplicitPrelude #-}

{-|
Module      : Random
Description : Custom random number generator
Copyright   : (c) 2023 Matteo Mariotti
License     : GNU GPL v.3
Maintainer  : matteomariotti0301@gmail.com
Stability   : experimental
Portability : POSIX

The implementation of a custom random number generator based on the xorshift32 algorithm.
-}
module Random (XORShift, mkRandom) where

{-@ LIQUID "--counter-examples" @-}

import Bits
import Prelude.Linear (($))
import System.Random
import Prelude hiding (xor, ($))

{-|
    The representation of the algorithm used for random number generation.
-}
newtype XORShift = Random Word32

{-|
    Creates a random generator using the parameter as seed.
-}

{-@ mkRandom :: Word32 -> XORShift @-}
mkRandom :: Word32 -> XORShift
mkRandom = Random

{-@ getRandom :: XORShift -> (Word32, XORShift) @-}
getRandom :: XORShift -> (Word32, XORShift)
getRandom (Random s) = (s, Random $ xorshift32 s)
  where
    xorshift32 x =
        let
            a = x `xor` (x `shiftL` 13)
            b = a `xor` (a `shiftR` 17)
            c = b `xor` (b `shiftL` 5)
         in
            c

instance RandomGen XORShift where
    {-@ lazy genWord32 @-}
    genWord32 :: XORShift -> (Word32, XORShift)
    genWord32 = getRandom

    split :: XORShift -> (XORShift, XORShift)
    split _ = error "No sound split implementation"
