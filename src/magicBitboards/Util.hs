module Util where

import Bits
import Common

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
             in
                if s $&$ (1 `shiftL` count) /= 0
                    then occupancy_helper (count - 1) (occ $|$ (1 `shiftL` square)) (clearBit a' square)
                    else occupancy_helper (count - 1) (occ) (clearBit a' square)

fileA :: Int
fileA = file2Int FA

fileB :: Int
fileB = file2Int FB

fileG :: Int
fileG = file2Int FG

fileH :: Int
fileH = file2Int FH

rank1 :: Int
rank1 = rank2Int R1

rank8 :: Int
rank8 = rank2Int R8