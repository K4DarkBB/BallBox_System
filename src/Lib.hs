module Lib where

import Control.Monad
import Data.Bits
import System.Random

data Ball = None | Exist | Will deriving (Eq, Enum)

type Boxes = [Ball]

instance Show Ball where
  show None = "□"
  show Exist = "■"
  show Will = "□"

boxUpdate :: Boxes -> Boxes
boxUpdate [] = []
boxUpdate [a] = [a]
boxUpdate (Exist : x) = None : boxUpdate (take c x <> [Will] <> drop (c + 1) x)
  where
    getExCount [] = 0
    getExCount (None : _) = 0
    getExCount (_ : x) = 1 + getExCount x
    c = getExCount x
boxUpdate (e : x) = e : boxUpdate x

boxUpdate2D :: [Boxes] -> [Boxes]
boxUpdate2D [] = []

finalize b = f <$> b <> [None]
  where
    f Will = Exist
    f e = e

showBox :: Boxes -> String
showBox b = init $head . show <$> b

toBox :: String -> Boxes
toBox s = tob <$> s
  where
    tob 'o' = Exist
    tob _ = None

randomBox :: Int -> IO Boxes
randomBox 0 = return []
randomBox len = randomRIO r >>= f
  where
    r = flip unsafeShiftL len <$> (1, 2 :: Int)
    f b = return $take (len + 1) $ toEnum . fromEnum . testBit b <$> [0 .. finiteBitSize b - 1]
