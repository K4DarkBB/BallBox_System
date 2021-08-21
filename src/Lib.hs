{-# LANGUAGE LambdaCase #-}

module Lib where

import Data.Bits
import Data.Word (Word8)
import System.Random

boxUpdate :: [Int] -> [Int]
boxUpdate = go []
  where
    go ls [] = reverse ls
    go ls (2 : rs) = go (1 : ls) rs
    go ls (1 : 0 : rs) = go (1 : 0 : ls) rs
    go ls (1 : rs) = let (ones, after) = break (== 0) rs in go (0 : ls) (ones <> [2] <> (if null after then [] else tail after))
    go ls (r : rs) = go (r : ls) rs

-- boxUpdate2D :: [Boxes] -> [Boxes]
-- boxUpdate2D [] = []

randomBox :: Int -> IO [Int]
randomBox 0 = return []
randomBox len = randomRIO range >>= f
  where
    range = flip unsafeShiftL len <$> (1, 2 :: Int) -- make tuple
    f b = return $ take (len + 1) $ fromEnum . testBit b <$> [0 .. finiteBitSize b - 1]

fromBit :: Integer -> [Word8]
fromBit 0 = []
fromBit b = fromIntegral (b .&. 1) : fromBit (b `shiftR` 1)
