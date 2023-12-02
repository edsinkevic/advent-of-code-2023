import qualified Shared

main :: IO ()
main = do
  content <- readFile "testdata.txt"
  let result = foldl accumulate 0 $ lines content
  _ <- print result
  return ()


accumulate :: Integer -> String -> Integer
accumulate currentSum line = currentSum + deducePower line

deducePower :: String -> Integer
deducePower = 
  minPower 0 0 0
  . filter (/= ' ') 
  . Shared.toColon

minPower :: Integer -> Integer -> Integer -> String -> Integer 
minPower r g b line =
  case line of
    (';':xs) -> minPower' r g b xs
    (',':xs) -> minPower' r g b xs
    [] -> r * g * b
    _ -> minPower' r g b line

minPower' :: Integer -> Integer -> Integer -> String -> Integer
minPower' r g b line =
  let (amount, color, afterColor) = Shared.readPair line
      (nr, ng, nb) = changeSet r g b amount color in
    minPower nr ng nb afterColor

changeSet r g b amount color = 
  case color of
    "red" -> (max r amount, g, b)
    "green" -> (r, max g amount, b)
    "blue" -> (r, g, max b amount)
