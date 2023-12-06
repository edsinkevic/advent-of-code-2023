module Shared where

naiveCalculation :: Integer -> (Integer, Integer) -> [Integer]
naiveCalculation holdTime pair@(time, distance) = 
  let (result, nextHoldTime, beats) = calculate time distance holdTime in
    if not beats
      then naiveCalculation nextHoldTime pair 
      else naiveCalculation' pair nextHoldTime [result]

naiveCalculation' :: (Integer, Integer) -> Integer -> [Integer] -> [Integer]
naiveCalculation' pair@(time, distance) holdTime results = 
  let (result, nextHoldTime, beats) = calculate time distance holdTime in
    if beats
      then naiveCalculation' pair nextHoldTime (result:results)
      else results

calculate :: Integer -> Integer -> Integer -> (Integer, Integer, Bool)
calculate time distance holdTime =
  let result = holdTime * (time - holdTime)
      nextHoldTime = holdTime + 1
      beats = result > distance in
    (result, nextHoldTime, beats)
