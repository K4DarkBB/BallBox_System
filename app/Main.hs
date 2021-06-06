module Main where

import Lib
import System.Environment (getArgs)
import qualified UI.HSCurses.Curses as C
import qualified UI.HSCurses.CursesHelper as Ch

count = 30

--boxes = toBox "oooo..oo...ooo..o.o." --fS ■ OS □.

main :: IO ()
main = do
  a <- getArgs
  bx <- if null a then randomBox 20 else pure $ toBox $ head a <> "."
  Ch.start
  mainloop bx count
  C.refresh
  C.getCh
  Ch.end

mainloop :: Boxes -> Int -> IO ()
mainloop _ 0 = putStrLn "end."
mainloop b c = do
  C.wAddStr C.stdScr $ '>' : showBox b <> "\n"
  mainloop (tail . finalize . boxUpdate $b) $c -1
