module Shared where

difference :: [Integer] -> [Integer] -> [Integer]
difference [b] accum = reverse accum
difference (a:b:history) accum = difference (b:history) $ (b - a):accum
