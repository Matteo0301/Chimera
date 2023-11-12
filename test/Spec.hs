module Spec (sig) where

-- import Bitboard
import BitboardTest
import QuickSpec
import Test.Tasty.QuickCheck as QC
import Unsafe.Linear (toLinear)

sig :: [Sig]
sig =
    [ mono @Bitboard,
      con "getPopulation" (toLinear getPopulation :: Bitboard -> Int),
      con "emptyBoard" (emptyBoard :: Bitboard)
    ]