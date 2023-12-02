module Shared where

readPair :: String -> (Integer, String, String)
readPair line = 
  let (amountStr, afterAmount) = span isNumber line 
      (color, afterColor) = span isColorLetter afterAmount in
    (read amountStr, color, afterColor)

toColon :: String -> String
toColon = tail . dropWhile (/= ':')

isNumber :: Char -> Bool
isNumber c = c >= '0' && c <= '9'

isLetter :: Char -> Bool
isLetter = not . isNumber

isNotSpecialCharacter :: Char -> Bool
isNotSpecialCharacter c = c /= ';' && c /= ','

isColorLetter :: Char -> Bool
isColorLetter c = isLetter c && isNotSpecialCharacter c
