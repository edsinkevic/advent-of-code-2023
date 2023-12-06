import Debug.Trace (traceShowId)

import qualified Shared

main :: IO ()
main = print . solve . lines =<< readFile "data.txt"

solve :: [String] -> Int
solve [times, distances] = 
  length
  . Shared.naiveCalculation 1
  $ (parseLine 6 times, parseLine 10 distances)

parseLine :: Int -> String -> Integer
parseLine n line = read . filter (/= ' ') $ drop n line
