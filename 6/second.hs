import qualified Shared

main :: IO ()
main = print . solve . lines =<< readFile "data.txt"

solve :: [String] -> Integer
solve [times, distances] = 
  Shared.calculate (parseLine 6 times) (parseLine 10 distances)

parseLine :: Int -> String -> Integer
parseLine n line = read . filter (/= ' ') $ drop n line
