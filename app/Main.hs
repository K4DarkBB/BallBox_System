module Main where

import Control.Monad (unless)
import Data.Function (fix)
import Lib (boxUpdate, randomBox)

--fS ■ OS □.

main :: IO ()
main = do
  boxes <- randomBox 50
  flip fix (dropWhile (== 0) boxes) $ \loop b -> do
    let next = boxUpdate b
    putStr "\x1b[1J"
    putStrLn $ fromBox next
    c <- getChar
    unless (c == 'q') $ loop next

toBox :: [Char] -> [Int]
toBox = map $ fromEnum . (== 'o')

fromBox :: [Int] -> [Char]
fromBox = map $ \x -> if x == 1 then '■' else ' '
