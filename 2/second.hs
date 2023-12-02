import Debug.Trace

main :: IO ()
main = do
  content <- readFile "data.txt"
  let result = foldl accumulate 0 $ lines content
  _ <- print result
  return ()


accumulate :: Integer -> String -> Integer
accumulate currentSum line = currentSum + deducePower line

deducePower :: String -> Integer
deducePower = 
  minPower 0 0 0 
  . filter (/= ' ') 
  . toColon

minPower :: 
  Integer -> Integer -> Integer -> String -> Integer 
minPower r g b [] =
  r * g * b
minPower r g b (';':xs) =
  minPower' r g b xs
minPower r g b (',':xs) =
  minPower' r g b xs
minPower r g b xs =
  minPower' r g b xs

minPower' :: Integer -> Integer -> Integer -> String -> Integer
minPower' r g b line =
  let (afterAmount, amount) = readAmount line
      (afterColor, color) = readColor afterAmount in
    case color of
      "red" -> 
        minPower (max r amount) g b afterColor
      "green" -> 
        minPower r (max g amount) b afterColor
      "blue" -> 
        minPower r g (max b amount) afterColor

readAmount :: String -> (String, Integer)
readAmount line = 
  (dropWhile isNumber line, read $ takeWhile isNumber line)

readColor :: String -> (String, String)
readColor line = 
  (
    dropWhile isColorLetter line, 
    takeWhile isColorLetter line
  )

dropAmount  :: String -> String
dropAmount = dropWhile isNumber

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

