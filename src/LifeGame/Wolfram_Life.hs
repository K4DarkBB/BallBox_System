module LifeGame.Wolfram_Life where

import Control.Arrow
import Data.Bits
import Data.Word (Word8)
import LifeGame.DataType

type Rule = Word8

rule :: Rule -> Z Bool -> Bool
rule w (Z (l : _) c (r : _)) = testBit w (sb 2 l .|. sb 1 c .|. sb 0 r)
  where
    sb n b = if b then bit n else 0
rule w (Z [] c rs) = rule w (Z [False] c rs)
rule w (Z ls c []) = rule w (Z ls c [False])

ruleR :: Rule -> ZRing Bool -> Bool
ruleR w (ZR l c (r : _)) = testBit w (sb 2 l .|. sb 1 c .|. sb 0 r)
  where
    sb n b = if b then bit n else 0
ruleR w (ZR l c []) = ruleR w (ZR l c [False])

testRule :: Rule -> [[Bool]]
testRule w = iterate (=>> rule w) >>> map (cut 70 70) $ start
  where
    start = Z (repeat False) True (repeat False)
    cut l r (Z ls c rs) = reverse (take l ls) <> [c] <> take r rs

testRuleR :: Rule -> [[Bool]]
testRuleR w = toL <$> iterate (=>> ruleR w) start
  where
    start = ZR False True (take 38 $ cycle [False,True,False])
    toL (ZR l c rs) = l : c : rs

viewRule :: Rule -> (Rule -> [[Bool]]) -> IO ()
viewRule w f = putStr . unlines . take 70 . map (map (\x -> if x then '■' else ' ')) $ f w
