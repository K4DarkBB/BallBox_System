module LifeGame where

import Data.Bits
import Data.Word

data LifeGame = LifeGame Integer Int Int

type Pixel64 = [Word64]

sizeData = 64

--789 46 123 .
neighbormoore :: (Num a, Bits a) => LifeGame -> Int -> Int -> [a]
neighbormoore (LifeGame bits w h) x y =
  [p (x -1) (y -1), p x (y -1), p (x + 1) (y -1), p (x -1) y, p x (y + 1), p (x + 1) (y -1), p (x + 1) y, p (x + 1) (y + 1), p x y]
  where
    p i j = fromIntegral $ pixel64 bits (i .&. finiteBitSize w) * w + (i .&. finiteBitSize j)

lifeUpdate :: (Num a, Bits a) => LifeGame -> a -> Integer
lifeUpdate d@(LifeGame bits w h) c = undefined

-- return s2,s3.
sumBits :: (Num a, Bits a) => [a] -> (a, a)
sumBits x = (x1 .&. complement a3, x1 .&. a3)
  where
    p = (x !!)
    xab = p 0 .&. p 1
    xcd = p 2 .&. p 3
    xef = p 4 .&. p 5
    xgh = p 6 .&. p 7
    a1 = p 0 `xor` p 1
    c1 = p 2 `xor` p 3
    e1 = p 4 `xor` p 5
    g1 = p 6 `xor` p 7
    d = a1 .&. c1
    a2 = a1 `xor` c1
    c2 = xab .&. xcd
    b1 = xab `xor` xcd `xor` d
    h = e1 .&. g1
    e2 = e1 `xor` g1
    g2 = xef .&. xgh
    f1 = xef `xor` xgh `xor` h
    d1 = a2 .&. e2
    a3 = a2 `xor` e2
    h1 = b1 .&. f1
    b2 = b1 `xor` f1
    h2 = h1 .|. b2 .&. d1
    b3 = b2 `xor` d1
    c3 = c2 `xor` g2 `xor` h2
    x1 = b3 .&. complement c3

pixel :: (Num a, Bits a) => LifeGame -> Int -> Int -> a
pixel (LifeGame bits width height) x y = fromIntegral $fromEnum $ testBitDefault bits $ u * width + v
  where
    u = x .&. finiteBitSize width
    v = y .&. finiteBitSize height

pixel64 :: (Num a, Bits a) => Integer -> Int -> a
pixel64 bits c = fromIntegral $ shiftR bits (bs - c) .&. shiftL 2 sizeData -1
  where
    Just bs = bitSizeMaybe bits

toPixel64 :: Integer -> Pixel64
toPixel64 0 = [0]
toPixel64 bits = undefined -- reverse $! fromInteger <$> bits .&. 2 `unsafeShiftL` sizeData -1 : toPixel64 $ bits `unsafeShiftR` sizeData

fromPixel64 :: Pixel64 -> Integer
fromPixel64 = fromIntegral . foldr (\p -> (.|.) (p `unsafeShiftL` sizeData)) 0
