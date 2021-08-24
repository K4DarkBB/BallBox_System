module Lib where

import Data.Bits (Bits (bit, shiftR, testBit, (.&.), (.|.)))
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import Data.Word (Word8)
import System.Random (randomRIO)

boxUpdate :: ByteString -> ByteString
boxUpdate = B.pack . go [] . unpackBit
  where
    go ls [] = reverse ls
    go ls (2 : rs) = go (1 : ls) rs
    go ls (1 : 0 : rs) = go (1 : 0 : ls) rs
    go ls (1 : rs) = let (ones, after) = break (== 0) rs in go (0 : ls) (ones <> [2] <> (if null after then [] else tail after))
    go ls (r : rs) = go (r : ls) rs

{- boxUpdateZ :: ZRing Word8 -> ZRing Word8
boxUpdateZ = undefined -}

randomBox :: Int -> IO ByteString
randomBox 0 = pure B.empty
randomBox len = fromBit <$> randomRIO (bit len, bit (len + 1))

fromBit :: Integer -> ByteString
fromBit = B.reverse . B.pack . go
  where
    go 0 = []
    go b = fromIntegral (b .&. 255) : go (b `shiftR` 8)

packBit :: [Bool] -> ByteString
packBit = B.pack . fmap fromIntegral . go
  where
    go [] = []
    go ls
      | length ls < 8 = pure . foldl1 (\x y -> x * 2 .|. y) $ fmap fromEnum ls
      | otherwise = let (x, xs) = splitAt 8 ls in foldl1 (\x1 y -> x1 * 2 .|. y) (fmap fromEnum x) : go xs

unpackBit :: ByteString -> [Word8]
unpackBit = fmap (fromIntegral . fromEnum) . concatMap go . B.unpack
  where
    go :: Word8 -> [Bool]
    go w = testBit w <$> [1 .. 7]
