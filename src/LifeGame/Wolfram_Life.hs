module LifeGame.Wolfram_Life where

import Control.Applicative
import Control.Monad
import Data.Bits

class Functor w => Comonad w where
  extract :: w a -> a
  duplicate :: w a -> w (w a)

  extend :: (w b -> a) -> w b -> w a
  extend f = fmap f . duplicate

-- zipper list
data Z a = Z [a] a [a]

left, right :: Z a -> Z a
left (Z (l : ls) c rs) = Z ls l (c : rs)
right (Z ls c (r : rs)) = Z (c : ls) r rs

instance Functor Z where
  fmap f (Z ls c rs) = Z (f <$> ls) (f c) (f <$> rs)

instance Comonad Z where
  extract (Z _ a _) = a
  duplicate z = Z (iterate1 left z) z (iterate1 right z)
    where
      iterate1 :: (a -> a) -> a -> [a]
      iterate1 f = tail . iterate f
