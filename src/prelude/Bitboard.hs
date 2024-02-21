{-|
Module      : Bitboard
Description : Module for bitboards representation and operations
Copyright   : (c) 2023-2024 Matteo Mariotti
License     : GNU GPL v.3
Stability   : experimental
Portability : POSIX

This module export the 'Bitboard' type and the functions to operate on its squares.
-}
{- FOURMOLU_DISABLE -}
{-|
-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{- FOURMOLU_ENABLE -}
module Bitboard
    ( Bitboard
    , Square (..)
    , File (..)
    , Rank (..)
    , getSquare
    , setSquare
    , (<<>>)
    , unsetSquare
    , population
    , emptyBoard
    , initialBoard
    , square2Index
    , bb
    , maskFile
    , maskRank
    , bbPop
    , printBitboard
    ) where

import Bits

import Common
import Control.Exception (assert)
import GHC.Exts
import Prelude.Linear (($))
import Prelude hiding (toList, ($))

{-@ LIQUID "--counter-examples" @-}
{-@ LIQUID "--reflection" @-}
{-@ LIQUID "--ple" @-}

{-@ type Pop = {x:Int | x >= 0 && x<= 64} @-}
{-@ type Index = {x:Int | x >= 0 && x<= 63} @-}

-- >>> 3 + 3
-- 6

{-@ data Bitboard' [piece_number] = Bitboard (bb :: {x:Int | pop x <= 32}) @-}

{-|
    The 'Bitboard' type is a newtype wrapper around 'Int' that represents a bitboard.
    The bitboard is a 64-bit integer where each bit represents a square on the board.
    The least significant bit represents the square h1, the most significant bit represents the square a8.
    The bitboard is stored in little endian order, so the first 8 bits represent the first row of the board.
-}
newtype Bitboard' = Bitboard {bb :: Int} deriving (Eq, Show, Ord)

{-@ type Bitboard = {x:Bitboard' | bbPop x <= 32} @-}

type Bitboard = Bitboard'

{-@ fail wrongBitboard @-}
{-@ wrongBitboard :: Bitboard @-}
wrongBitboard :: Bitboard
wrongBitboard = Bitboard 0x00FFFFFFFFFFFFFF

instance IsList Bitboard where
    type Item Bitboard = Square

    toList :: Bitboard -> [Item Bitboard]
    toList bb@(Bitboard bb')
        | bb == emptyBoard = []
        | otherwise =
            let lastSquare = index2Square (countTrailingZeros bb')
             in lastSquare : toList (unsetSquare bb lastSquare)

    fromList :: [Item Bitboard] -> Bitboard
    fromList = foldl' setSquare emptyBoard

printBitboard :: Bitboard -> Text
printBitboard (Bitboard bb) = showBits bb

{-@ emptyBoard :: {x:Bitboard | bbPop x == 0} @-}

{-|
    Represents the empty board.
-}
emptyBoard :: Bitboard
emptyBoard = Bitboard 0

{-|
    Represents the starting position.
-}
initialBoard :: Bitboard
initialBoard = Bitboard (-0x0000FFFFFFFF0001)

{-# WARNING
    bbPop
    "This is here only to be used by LiquidHaskell. It should not be used in real code."
    #-}
{-@ inline bbPop @-}
bbPop :: Bitboard -> Int
bbPop (Bitboard bb) = pop bb

{-@ measure piece_number :: Bitboard -> Int
            piece_number (Bitboard b) = pop b @-}

{-|
    Returns the number of squares occupied in the bitboard.
-}
population :: Bitboard -> Int
population (Bitboard bb) = popCount bb

{-|
    Returns whether the square is set in the bitboard
-}
getSquare :: Bitboard -> Square -> Bool
getSquare (Bitboard bb) sq = testBit bb (square2Index sq)

{-|
    Sets the square in the bitboard. If the new bitboard has more that 32 squares occupied, returns the old one.
-}
setSquare :: Bitboard -> Square -> Bitboard
setSquare (Bitboard bb) sq =
    assert
        (pop (setBit bb (square2Index sq)) <= 32)
        (Bitboard $ setBit bb (square2Index sq))

{-|
    Sets a certain square in the board as empty
-}
unsetSquare :: Bitboard -> Square -> Bitboard
unsetSquare (Bitboard bb) sq = Bitboard $ clearBit bb (square2Index sq)

{-|
    The operator version of 'setSquare'. It doesn't have a linear type because it is meant to be used mainly for testing (so it's easier to use through folds and maps).
-}
(<<>>) :: Bitboard -> Square -> Bitboard
bb <<>> i = setSquare bb i

infixl 8 <<>>
