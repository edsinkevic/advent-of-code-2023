import Data.Set (Set, intersection, fromList)
import qualified Shared as Shared

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
  . Shared.parseNumbers
  $ Shared.skipToColon line

deducePoints :: Int -> Integer
deducePoints matchedCount = 
  case matchedCount of
    0 -> 0
    _ -> 2 ^ (matchedCount - 1)
