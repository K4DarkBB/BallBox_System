{-# LANGUAGE LambdaCase #-}

module Lib where

import Data.Bits
import System.Random

boxUpdate :: [Int] -> [Int]
boxUpdate = go []
  where
    go ls [] = reverse ls
    go ls (2 : rs) = go (1 : ls) rs
    go ls (1 : 0 : rs) = go (1 : 0 : ls) rs
    go ls (1 : rs) = let (ones, after) = break (== 0) rs in if null after then go (0 : ls) (ones <> [2]) else go (0 : ls) (ones <> [2] <> tail after)
    go ls (r : rs) = go (r : ls) rs

-- boxUpdate2D :: [Boxes] -> [Boxes]
-- boxUpdate2D [] = []

randomBox :: Int -> IO [Int]
randomBox 0 = return []
randomBox len = randomRIO range >>= f
  where
    range = flip unsafeShiftL len <$> (1, 2 :: Int) -- make tapule
    f b = return $ take (len + 1) $ fromEnum . testBit b <$> [0 .. finiteBitSize b - 1]
