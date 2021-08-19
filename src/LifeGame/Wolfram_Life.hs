module LifeGame.Wolfram_Life where

import Data.Bits
import LifeGame.DataType

type Rule = Int

toRule :: Int -> Rule
toRule = (255 .&.)

updateLife :: Rule -> Z Bool -> Z Bool
updateLife = extend . (. neighborhoods) . computeNext

--111 110 101 100 011 010 001 000
computeNext :: Rule -> (Bool, Bool, Bool) -> Bool
computeNext r (bl, bc, br) = toEnum $ 1 .&. r `shiftR` offset
  where
    il, ic, ir :: Int
    (il, ic, ir) = (fromEnum bl, fromEnum bc, fromEnum br)

    offset :: Int
    offset = il `shiftL` 2 .&. ic `shiftL` 1 .&. ir
