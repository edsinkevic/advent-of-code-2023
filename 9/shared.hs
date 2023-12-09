module Shared where

differences :: [Integer] -> [[Integer]] -> [[Integer]]
differences history accum =
  if shouldEnd then reverse (diff:accum) else differences diff (diff:accum)
    where diff = difference history []
          shouldEnd = all (== 0) diff
  
difference :: [Integer] -> [Integer] -> [Integer]
difference [b] accum = reverse accum
difference (a:b:history) accum = difference (b:history) $ (b - a):accum
