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

finalize [] = [None]
finalize (Will : x) = Exist : finalize x
finalize (e : x) = e : finalize x

showBox :: [Ball] -> String
showBox [] = []
showBox [_] = []
showBox (Exist : x) = 'o' : showBox x
showBox (_ : x) = '.' : showBox x

toBox :: String -> [Ball]
toBox [] = []
toBox ('o' : s) = Exist : toBox s
toBox (_ : s) = None : toBox s

----------------------------------------

data LifeGame = LifeGame Integer Int Int

pixel :: (Num a, Bits a) => LifeGame -> Int -> Int -> a
pixel (LifeGame bits width height) x y = if testBitDefault bits $ u * width + v then 1 else 0
  where
    u = x .&. finiteBitSize width
    v = y .&. finiteBitSize height

--789 46 123.
neighbormoore :: (Num a, Bits a) => LifeGame -> Int -> Int -> [a]
neighbormoore lg@(LifeGame _ width height) x y =
  [p (x -1) (y -1), p x (y -1), p (x + 1) (y -1), p (x -1) y, p x (y + 1), p (x + 1) (y -1), p (x + 1) y, p (x + 1) (y + 1)]
  where
    p = pixel lg

sumBits :: (Num a, Bits a) => [a] -> (a, a)
sumBits x = (0, 0)
  where
    p = (x !!)
    xab = p 0 .&. p 1
    xcd = p 2 .&. p 3
    xef = p 4 .&. p 5
    xgh = p 6 .&. p 7

lifeUpdate :: Bits a => Integer -> a -> Integer
lifeUpdate 0 _ = 0
