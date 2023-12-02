import Debug.Trace

main :: IO ()
main = do
  content <- readFile "data.txt"
  let result = foldl accumulate 0 $ lines content
  _ <- print result
  return ()


accumulate :: Integer -> String -> Integer
accumulate currentSum line = currentSum + getIdIfPossible line

getIdIfPossible :: String -> Integer
getIdIfPossible line = 
  if isPossible line
    then read . takeWhile isNumber $ dropGame line
    else 0

dropGame :: String -> String
dropGame = dropWhile isLetter

isPossible :: String -> Bool
isPossible = 
  isPossibleAccumulate 0 0 0 
  . filter (\x -> x /= ' ') 
  . toColon

isPossibleAccumulate :: Integer -> Integer -> Integer -> String -> Bool
isPossibleAccumulate r g b [] =
  checkConstraint r g b

isPossibleAccumulate r g b (';':xs) =
  if checkConstraint r g b 
    then isPossibleAccumulate' 0 0 0 xs else False

isPossibleAccumulate r g b (',':xs) =
  isPossibleAccumulate' r g b xs

isPossibleAccumulate r g b xs =
  isPossibleAccumulate' r g b xs
  
checkConstraint :: Integer -> Integer -> Integer -> Bool
checkConstraint r g b = r <= 12 && g <= 13 && b <= 14

isPossibleAccumulate' :: Integer -> Integer -> Integer -> String -> Bool
isPossibleAccumulate' r g b line =
  let (afterAmount, amount) = readAmount line
      (afterColor, color) = readColor afterAmount in
    case color of
      "red" -> 
        isPossibleAccumulate (r + amount) g b afterColor
      "green" -> 
        isPossibleAccumulate r (g + amount) b afterColor
      "blue" -> 
        isPossibleAccumulate r g (b + amount) afterColor

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

