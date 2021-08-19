module Main where

import Data.Function (fix)
import Lib (boxUpdate, randomBox)
import Control.Monad (unless)

--fS ■ OS □.

main :: IO ()
main = do
  boxes <- randomBox 40
  flip fix (dropWhile (== 0) boxes) $ \loop b -> do
    let next = boxUpdate b
    putStr "\x1b[1J"
    putStrLn $ fromBox next
    c <- getChar
    unless (c == 'q') . loop $ tail next

toBox :: [Char] -> [Int]
toBox = map $ fromEnum . (== 'o')

fromBox :: [Int] -> [Char]
fromBox = map $ \x -> if x == 1 then '■' else '□'
