module LifeGame.DataType where

class Functor w => Comonad w where
  extract :: w a -> a
  duplicate :: w a -> w (w a)

  extend :: (w b -> a) -> w b -> w a
  extend f = fmap f . duplicate

-- zipper list
data Z a = Z [a] a [a]

left, right :: Z a -> Z a
left (Z (l : ls) c rs) = Z ls l (c : rs)
left z = z
right (Z ls c (r : rs)) = Z (c : ls) r rs
right z = z

instance Functor Z where
  fmap f (Z ls c rs) = Z (map f ls) (f c) (map f rs)

instance Comonad Z where
  extract (Z _ a _) = a
  duplicate z = Z (iterate1 left z) z (iterate1 right z)
    where
      iterate1 :: (a -> a) -> a -> [a]
      iterate1 f = tail . iterate f

instance Show a => Show (Z a) where
  show (Z l c r) = show (reverse l) <> show c <> show r

neighborhoods :: Z a -> (a, a, a)
neighborhoods (Z (l : _) c (r : _)) = (l, c, r)
neighborhoods (Z _ c _) = (c, c, c)
