import Data.Set (Set, intersection, fromList)

main :: IO ()
main = print =<< solve <$> readFile "data.txt"

solve :: String -> Integer
solve content = foldl accumulatePoints 0 $ lines content

accumulatePoints :: Integer -> String -> Integer
accumulatePoints points line = points + calculatePoints line

calculatePoints :: String -> Integer
calculatePoints line = 
  deducePoints
  . length 
  . uncurry intersection
  . parseNumbers
  $ skipToColon line

deducePoints :: Int -> Integer
deducePoints matchedCount = 
  case matchedCount of
    0 -> 0
    _ -> 2 ^ (matchedCount - 1)

skipToColon :: String -> String
skipToColon line = tail $ dropWhile (/= ':') line

parseNumbers :: String -> (Set Integer, Set Integer)
parseNumbers line = 
  (
    parseNumbers' $ takeWhile notStick line, 
    parseNumbers' . tail $ dropWhile notStick line
  )

notStick :: Char -> Bool
notStick = (/= '|')

parseNumbers' :: String -> Set Integer
parseNumbers' line = fromList $ read <$> words line
