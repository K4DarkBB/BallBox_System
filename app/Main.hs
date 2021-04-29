module Main where

import Lib
import System.Environment (getArgs)

count = 30

boxes = toBox "oooo..oo...ooo..o.o."

main :: IO ()
main = do
  a <- getArgs
  let bx =if null a then boxes else finalize $ toBox $ head a
  mainloop bx count

mainloop :: [Ball] -> Int -> IO ()
mainloop _ 0 = putStrLn "end."
mainloop b c = do
  putStrLn $ ">" <> showBox b
  mainloop (tail $finalize $update b) $c -1

update :: [Ball] -> [Ball]
update [] = []
update [a] = [a]
update (Exist : x) = None : update (take c x <> [Will] <> drop (c + 1) x)
  where
    getExCount [] = 0
    getExCount (None : _) = 0
    getExCount (_ : x) = 1 + getExCount x
    c = getExCount x
update (e : x) = e : update x

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
