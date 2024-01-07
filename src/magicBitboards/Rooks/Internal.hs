{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use camelCase" #-}
{-# HLINT ignore "Redundant bracket" #-}

{-@ LIQUID "--reflection" @-}
{-@ LIQUID "--ple" @-}
-- {-@ LIQUID "--ple-with-undecided-guards" @-}
{-@ LIQUID "--extensionality" @-}
{-@ LIQUID "--counter-examples" @-}

module Rooks.Internal (maskRookAttacks, attacksOnTheFly, occupancy) where

import Bitboard
import Bits
import Common

type RookMask = Int

type Occupancy = Int

{-@ lazy mask_attacks_b @-}
mask_attacks_b :: Int -> RookMask
mask_attacks_b s
    | s $&$ m /= 0 = 0
    | otherwise = mask_attacks_b' (s `shiftR` 8)
  where
    m = rank2Int R1
    mask_attacks_b' 0 = 0
    mask_attacks_b' s'
        | s' $&$ m /= 0 = 0
        | otherwise = s' $|$ mask_attacks_b' (s' `shiftR` 8)

{-@ lazy mask_attacks_t @-}
mask_attacks_t :: Int -> RookMask
mask_attacks_t s
    | (s) $&$ m /= 0 = 0
    | otherwise = mask_attacks_t' (s `shiftL` 8)
  where
    m = rank2Int R8
    mask_attacks_t' 0 = 0
    mask_attacks_t' s'
        | s' $&$ m /= 0 = 0
        | otherwise = s' $|$ mask_attacks_t' (s' `shiftL` 8)

{-@ lazy mask_attacks_l @-}
mask_attacks_l :: Int -> RookMask
mask_attacks_l s
    | (s) $&$ m /= 0 = 0
    | otherwise = mask_attacks_l' (s `shiftL` 1)
  where
    m = file2Int FA
    mask_attacks_l' 0 = 0
    mask_attacks_l' s'
        | s' $&$ m /= 0 = 0
        | otherwise = s' $|$ mask_attacks_l' (s' `shiftL` 1)

{-@ lazy mask_attacks_r @-}
mask_attacks_r :: Int -> RookMask
mask_attacks_r s
    | (s) $&$ m /= 0 = 0
    | otherwise = mask_attacks_r' (s `shiftR` 1)
  where
    m = file2Int FH
    mask_attacks_r' 0 = 0
    mask_attacks_r' s'
        | s' $&$ m /= 0 = 0
        | otherwise = s' $|$ mask_attacks_r' (s' `shiftR` 1)

maskRookAttacks :: Square -> RookMask
maskRookAttacks s =
    let s' = squareMask s
     in mask_attacks_l s'
            $|$ mask_attacks_b s'
            $|$ mask_attacks_t s'
            $|$ mask_attacks_r s'

{-@ lazy attacks_b @-}
attacks_b :: Int -> Int -> RookMask
attacks_b occ s
    | s $&$ m /= 0 = 0
    | otherwise = attacks_b' (s `shiftR` 8)
  where
    m = rank2Int R1
    attacks_b' 0 = 0
    attacks_b' s'
        | s' $&$ m /= 0 = s'
        | occ $&$ s' /= 0 = s'
        | otherwise = s' $|$ attacks_b' (s' `shiftR` 8)

{-@ lazy attacks_t @-}
attacks_t :: Int -> Int -> RookMask
attacks_t occ s
    | (s) $&$ m /= 0 = 0
    | otherwise = attacks_t' (s `shiftL` 8)
  where
    m = rank2Int R8
    attacks_t' 0 = 0
    attacks_t' s'
        | s' $&$ m /= 0 = s'
        | occ $&$ s' /= 0 = s'
        | otherwise = s' $|$ attacks_t' (s' `shiftL` 8)

{-@ lazy attacks_l @-}
attacks_l :: Int -> Int -> RookMask
attacks_l occ s
    | (s) $&$ m /= 0 = 0
    | otherwise = attacks_l' (s `shiftL` 1)
  where
    m = file2Int FA
    attacks_l' 0 = 0
    attacks_l' s'
        | s' $&$ m /= 0 = s'
        | occ $&$ s' /= 0 = s'
        | otherwise = s' $|$ attacks_l' (s' `shiftL` 1)

{-@ lazy attacks_r @-}
attacks_r :: Int -> Int -> RookMask
attacks_r occ s
    | (s) $&$ m /= 0 = 0
    | otherwise = attacks_r' (s `shiftR` 1)
  where
    m = file2Int FH
    attacks_r' 0 = 0
    attacks_r' s'
        | s' $&$ m /= 0 = s'
        | occ $&$ s' /= 0 = s'
        | otherwise = s' $|$ attacks_r' (s' `shiftR` 1)

attacksOnTheFly :: Bitboard -> Square -> AttackBB
attacksOnTheFly occ s =
    let s' = squareMask s
        occ' = bb2Int occ
     in AttackBB $
            attacks_l occ' s'
                $|$ attacks_b occ' s'
                $|$ attacks_t occ' s'
                $|$ attacks_r occ' s'

{-@ ignore occupancy @-}
occupancy :: Int -> Int -> RookMask -> Occupancy
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
