module Main where

import Lib
import Control.Monad.State

count :: Int
count = 10

boxes :: [Ball]
boxes = [None, Exist, None]

main :: IO ()
main = print "a"

update :: [Ball] -> [Ball]
update [] = []
update [a]=[a]
update (None : x) =None : update x
update (Will : x) =Will : update x
update (Exist : x)=None :x
    where
        getEx xs@(None :x)=(x,xs)