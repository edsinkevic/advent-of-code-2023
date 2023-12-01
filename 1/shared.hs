module Shared where

isNumber :: Char -> Bool
isNumber c = c >= '0' && c <= '9'

calibrationDigits x = read [head x, last x]

calibration :: [Char] -> Integer
calibration = calibrationDigits . filter isNumber



