{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use camelCase" #-}
{-# HLINT ignore "Redundant bracket" #-}
module Rooks.Internal (maskRookAttacks, attacksOnTheFly) where

import Bitboard
import Bits
import Common

type BishopMask = Int

{-@ lazy mask_attacks_b @-}
mask_attacks_b :: Int -> BishopMask
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
mask_attacks_t :: Int -> BishopMask
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
mask_attacks_l :: Int -> BishopMask
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
mask_attacks_r :: Int -> BishopMask
mask_attacks_r s
    | (s) $&$ m /= 0 = 0
    | otherwise = mask_attacks_r' (s `shiftR` 1)
  where
    m = file2Int FH
    mask_attacks_r' 0 = 0
    mask_attacks_r' s'
        | s' $&$ m /= 0 = 0
        | otherwise = s' $|$ mask_attacks_r' (s' `shiftR` 1)

maskRookAttacks :: Square -> BishopMask
maskRookAttacks s =
    let s' = squareMask s
     in mask_attacks_l s'
            $|$ mask_attacks_b s'
            $|$ mask_attacks_t s'
            $|$ mask_attacks_r s'

{-@ lazy attacks_b @-}
attacks_b :: Int -> Int -> BishopMask
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
attacks_t :: Int -> Int -> BishopMask
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
attacks_l :: Int -> Int -> BishopMask
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
attacks_r :: Int -> Int -> BishopMask
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