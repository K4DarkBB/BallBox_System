module Main where

import Lib
import System.Environment (getArgs)

count = 30

boxes = toBox "oooo..oo...ooo..o.o."--fS ■

main :: IO ()
main = do
  a <- getArgs
  let bx =if null a then boxes else finalize $ toBox $ head a
  mainloop bx count

mainloop :: [Ball] -> Int -> IO ()
mainloop _ 0 = putStrLn "end."
mainloop b c = do
  putStrLn $ ">" <> showBox b
  mainloop (tail $finalize $boxUpdate b) $c -1
