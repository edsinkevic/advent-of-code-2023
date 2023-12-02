import qualified Shared

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
    then read . takeWhile Shared.isNumber $ dropGame line
    else 0

dropGame :: String -> String
dropGame = dropWhile Shared.isLetter

isPossible :: String -> Bool
isPossible =
  isPossibleAccumulate 0 0 0
  . filter (/= ' ')
  . Shared.toColon

isPossibleAccumulate :: Integer -> Integer -> Integer -> String -> Bool
isPossibleAccumulate r g b line =
  case line of
    [] -> checkConstraint r g b
    (';':xs) ->
      checkConstraint r g b && isPossibleAccumulate' 0 0 0 xs
    (',':xs) -> isPossibleAccumulate' r g b xs
    _ -> isPossibleAccumulate' r g b line

checkConstraint :: Integer -> Integer -> Integer -> Bool
checkConstraint r g b = r <= 12 && g <= 13 && b <= 14

isPossibleAccumulate' :: Integer -> Integer -> Integer -> String -> Bool
isPossibleAccumulate' r g b line =
  let (amount, color, afterColor) = Shared.readPair line
      (nr, ng, nb) = changeSet r g b amount color in
    isPossibleAccumulate nr ng nb afterColor

changeSet r g b amount color =
  case color of
    "red" ->
      (r + amount, g , b)
    "green" ->
      (r, g + amount, b)
    "blue" ->
      (r, g, b + amount)
