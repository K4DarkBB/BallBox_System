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
