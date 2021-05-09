module Main where

import Lib
import System.Environment (getArgs)

count = 30

boxes = toBox "oooo..oo...ooo..o.o." --fS ■ OS □.

main :: IO ()
main = do
  a <- getArgs
  bx <- if null a then randomBox 20 else return $ toBox $ head a <> "."
  mainloop bx count

mainloop :: [Ball] -> Int -> IO ()
mainloop _ 0 = putStrLn "end."
mainloop b c = do
  putStrLn $ ">" <> showBox b
  mainloop (tail . finalize . boxUpdate $b) $c -1
