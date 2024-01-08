module Util where

import Bits

type Occupancy = Int
type AttackMask = Int

{-@ ignore occupancy @-}
occupancy :: Int -> Int -> AttackMask -> Occupancy
occupancy _ 0 _ = 0
occupancy _ _ 0 = 0
occupancy s bits a = occupancy_helper bits 0 a
  where
    occupancy_helper count occ a'
        | count <= 0 = occ
        | otherwise =
            let
                square = countTrailingZeros a'
                occ' =
                    if s $&$ (1 `shiftL` count) /= 0
                        then occ $|$ (1 `shiftL` square)
                        else occ
             in
                occupancy_helper (count - 1) occ' (clearBit a' square)