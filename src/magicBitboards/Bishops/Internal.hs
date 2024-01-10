{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use camelCase" #-}
{-# HLINT ignore "Redundant bracket" #-}
module Bishops.Internal (maskBishopAttacks, attacksOnTheFly) where

import Bitboard
import Bits
import Common
import Util

type BishopMask = AttackMask

{-@ lazy mask_attacks_br @-}
mask_attacks_br :: Int -> BishopMask
mask_attacks_br s
    | s $&$ m /= 0 = 0
    | otherwise = mask_attacks_br' (s `shiftR` 9)
  where
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

attacksOnTheFly :: Bitboard -> Square -> AttackBB
attacksOnTheFly occ s =
    let s' = squareMask s
        occ' = bb occ
     in AttackBB $
            attacks_bl occ' s'
                $|$ attacks_br occ' s'
                $|$ attacks_tl occ' s'
                $|$ attacks_tr occ' s'

{-@ lazy attacks_br @-}
attacks_br :: Int -> Int -> BishopMask
attacks_br occ s
    | s $&$ m /= 0 = 0
    | otherwise = attacks_br' (s `shiftR` 9)
  where
    m = fileH $|$ rank8
    attacks_br' 0 = 0
    attacks_br' s'
        | s' $&$ m /= 0 = s'
        | occ $&$ s' /= 0 = s'
        | otherwise = s' $|$ attacks_br' (s' `shiftR` 9)

{-@ lazy attacks_tr @-}
attacks_tr :: Int -> Int -> BishopMask
attacks_tr occ s
    | (s) $&$ m /= 0 = 0
    | otherwise = attacks_tr' (s `shiftL` 7)
  where
    m = fileH $|$ rank8
    attacks_tr' 0 = 0
    attacks_tr' s'
        | s' $&$ m /= 0 = s'
        | occ $&$ s' /= 0 = s'
        | otherwise = s' $|$ attacks_tr' (s' `shiftL` 7)

{-@ lazy attacks_tl @-}
attacks_tl :: Int -> Int -> BishopMask
attacks_tl occ s
    | (s) $&$ m /= 0 = 0
    | otherwise = attacks_tl' (s `shiftL` 9)
  where
    m = fileA $|$ rank8
    attacks_tl' 0 = 0
    attacks_tl' s'
        | s' $&$ m /= 0 = s'
        | occ $&$ s' /= 0 = s'
        | otherwise = s' $|$ attacks_tl' (s' `shiftL` 9)

{-@ lazy attacks_bl @-}
attacks_bl :: Int -> Int -> BishopMask
attacks_bl occ s
    | (s) $&$ m /= 0 = 0
    | otherwise = attacks_bl' (s `shiftR` 7)
  where
    m = fileA $|$ rank1
    attacks_bl' 0 = 0
    attacks_bl' s'
        | s' $&$ m /= 0 = s'
        | occ $&$ s' /= 0 = s'
        | otherwise = s' $|$ attacks_bl' (s' `shiftR` 7)
