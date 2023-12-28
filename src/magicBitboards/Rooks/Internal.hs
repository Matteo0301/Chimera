{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use camelCase" #-}
{-# HLINT ignore "Redundant bracket" #-}
module Rooks.Internal where

import Bits
import Common

type BishopMask = Int

{-@ ignore mask_attacks_b @-}
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
