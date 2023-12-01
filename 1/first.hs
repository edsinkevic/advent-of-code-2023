main :: IO ()
main = do
  content <- readFile "data1.txt"
  let result = foldl accumulator 0 $ lines content
  _ <- print result
  return ()

accumulator :: Integer -> String -> Integer
accumulator currentSum line = currentSum + extractCalibration line

extractCalibration :: String -> Integer
extractCalibration = firstAndLastToDigit . filter isNumber

firstAndLastToDigit :: String -> Integer
firstAndLastToDigit x = read [head x, last x]

isNumber :: Char -> Bool
isNumber c = c >= '1' && c <= '9'

