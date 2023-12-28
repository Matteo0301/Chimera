{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use camelCase" #-}
{-# HLINT ignore "Redundant bracket" #-}
module Bishops.Internal where

import Bits
import Common
import Debug.Trace

type BishopMask = Int

{-@ ignore mask_attacks_br @-}
mask_attacks_br :: Int -> BishopMask
mask_attacks_br s
    | s $&$ m /= 0 = 0
    | otherwise = mask_attacks_br' (s `shiftR` 9)
  where
    fileH = file2Int FH
    rank8 = rank2Int R1
    m = fileH $|$ rank8
    mask_attacks_br' 0 = 0
    mask_attacks_br' s'
        | s' $&$ m /= 0 = 0
        | otherwise = s' $|$ mask_attacks_br' (s' `shiftR` 9)

{-@ lazy mask_attacks_tr @-}
mask_attacks_tr :: Int -> BishopMask
mask_attacks_tr s
    | (s) $&$ m /= 0 = 0
    | otherwise = mask_attacks_tr' (s `shiftL` 7)
  where
    fileH = file2Int FH
    rank8 = rank2Int R8
    m = fileH $|$ rank8
    mask_attacks_tr' 0 = 0
    mask_attacks_tr' s'
        | s' $&$ m /= 0 = 0
        | otherwise = s' $|$ mask_attacks_tr' (s' `shiftL` 7)

{-@ lazy mask_attacks_tl @-}
mask_attacks_tl :: Int -> BishopMask
mask_attacks_tl s
    | (s) $&$ m /= 0 = 0
    | otherwise = mask_attacks_tl' (s `shiftL` 9)
  where
    fileA = file2Int FA
    rank8 = rank2Int R8
    m = fileA $|$ rank8
    mask_attacks_tl' 0 = 0
    mask_attacks_tl' s'
        | s' $&$ m /= 0 = 0
        | otherwise = s' $|$ mask_attacks_tl' (s' `shiftL` 9)

{-@ lazy mask_attacks_bl @-}
mask_attacks_bl :: Int -> BishopMask
mask_attacks_bl s
    | (s) $&$ m /= 0 = 0
    | otherwise = mask_attacks_bl' (s `shiftR` 7)
  where
    fileA = file2Int FA
    rank1 = rank2Int R1
    m = fileA $|$ rank1
    mask_attacks_bl' 0 = 0
    mask_attacks_bl' s'
        | s' $&$ m /= 0 = 0
        | otherwise = s' $|$ mask_attacks_bl' (s' `shiftR` 7)

maskBishopAttacks :: Square -> BishopMask
maskBishopAttacks s =
    let s' = squareMask s
     in mask_attacks_bl s'
            $|$ mask_attacks_br s'
            $|$ mask_attacks_tl s'
            $|$ mask_attacks_tr s'