module Lib where

data Ball = None | Exist | Will

instance Show Ball where
  show None="."
  show Exist="o"
  show Will="w"
