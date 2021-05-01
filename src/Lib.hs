module Lib where

import Data.Bits
import Data.Word

data Ball = None | Exist | Will

instance Show Ball where
  show None = "."
  show Exist = "o"
  show Will = "w"

boxUpdate :: [Ball] -> [Ball]
boxUpdate [] = []
boxUpdate [a] = [a]
boxUpdate (Exist : x) = None : boxUpdate (take c x <> [Will] <> drop (c + 1) x)
  where
    getExCount [] = 0
    getExCount (None : _) = 0
    getExCount (_ : x) = 1 + getExCount x
    c = getExCount x
boxUpdate (e : x) = e : boxUpdate x

finalize b = f <$> b <> [None]
  where
    f Will = Exist
    f e = e

showBox :: [Ball] -> String
showBox b = init $ sb <$> b
  where
    sb Exist = '■'
    sb _ = '.'

toBox :: String -> [Ball]
toBox s = tob <$> s
  where
    tob 'o' = Exist
    tob _ = None

----------------------------------------

data LifeGame = LifeGame Integer Int Int

sizeData = 64

pixel :: (Num a, Bits a) => LifeGame -> Int -> Int -> a
pixel (LifeGame bits width height) x y = fromIntegral $fromEnum $ testBitDefault bits $ u * width + v
  where
    u = x .&. finiteBitSize width
    v = y .&. finiteBitSize height

pixel64 :: (Num a, Bits a) => LifeGame -> Int -> a
pixel64 (LifeGame bits w h) c = fromIntegral $ shiftR bits bs .&. shiftL 2 sizeData -1
  where
    bs = w * h - c

--789 46 123 .
neighbormoore :: (Num a, Bits a) => LifeGame -> Int -> Int -> [a]
neighbormoore lg@(LifeGame _ w h) x y =
  [p (x -1) (y -1), p x (y -1), p (x + 1) (y -1), p (x -1) y, p x (y + 1), p (x + 1) (y -1), p (x + 1) y, p (x + 1) (y + 1), p x y]
  where
    p i j = fromIntegral $ pixel64 lg (i .&. finiteBitSize w) * w + (i .&. finiteBitSize j)

lifeUpdate :: (Num a, Bits a) => LifeGame -> a -> Integer
lifeUpdate (LifeGame bits w h) c = 0
  where
    size = w * h
    n = fromIntegral $shiftR sizeData size

-- return s2,s3.
sumBits :: (Num a, Bits a) => [a] -> (a, a)
sumBits x = (x1 .&. complement a3, x1 .&. a3)
  where
    p = (x !!)
    xab = p 0 .&. p 1
    xcd = p 2 .&. p 3
    xef = p 4 .&. p 5
    xgh = p 6 .&. p 7
    a1 = p 0 `xor` p 1
    c1 = p 2 `xor` p 3
    e1 = p 4 `xor` p 5
    g1 = p 6 `xor` p 7
    d = a1 .&. c1
    a2 = a1 `xor` c1
    c2 = xab .&. xcd
    b1 = xab `xor` xcd `xor` d
    h = e1 .&. g1
    e2 = e1 `xor` g1
    g2 = xef .&. xgh
    f1 = xef `xor` xgh `xor` h
    d1 = a2 .&. e2
    a3 = a2 `xor` e2
    h1 = b1 .&. f1
    b2 = b1 `xor` f1
    h2 = h1 .|. b2 .&. d1
    b3 = b2 `xor` d1
    c3 = c2 `xor` g2 `xor` h2
    x1 = b3 .&. complement c3
