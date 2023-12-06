module Shared where

calculate :: Integer -> Integer -> Integer
calculate time distance =
  let quad = quadratic time distance
      x1 = floor $ quad (+)
      x2 = ceiling $ quad (-) in
    (abs $ x2 - x1) - 1

quadratic time distance operand  = 
  let b = time
      c = distance
      det = b^2 - 4 * c in
    fromIntegral (-b) `operand` sqrt (fromIntegral det) / (-2)
