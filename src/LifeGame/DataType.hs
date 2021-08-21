{-# LANGUAGE DeriveFunctor #-}

module LifeGame.DataType where

class Functor w => Comonad w where
  extract :: w a -> a
  duplicate :: w a -> w (w a)

  extend :: (w b -> a) -> w b -> w a
  extend f = fmap f . duplicate
  (=>>) :: w a -> (w a -> b) -> w b
  (=>>) = flip extend

-- zipper list
data Z a = Z [a] a [a]
  deriving (Functor, Eq, Ord)

left, right :: Z a -> Z a
left (Z (l : ls) c rs) = Z ls l (c : rs)
left z = z
right (Z ls c (r : rs)) = Z (c : ls) r rs
right z = z

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

data ZRing a = ZR a a [a]
  deriving (Functor, Show, Eq, Ord)

next :: ZRing a -> ZRing a
next (ZR b c (r : rs)) = ZR c r $ rs <> [b]
next _ = error "empty list"

prev :: ZRing a -> ZRing a
prev (ZR b c rs) = ZR (last rs) b (c : init rs)

instance Comonad ZRing where
  extract (ZR _ a _) = a
  duplicate z@(ZR _ _ rs) = ZR (prev z) z (iterateR (length rs - 1) z)
    where
      iterateR :: Int -> ZRing a -> [ZRing a]
      iterateR 0 zs = [zs]
      iterateR n zs = zs : iterateR (n - 1) (next zs)
