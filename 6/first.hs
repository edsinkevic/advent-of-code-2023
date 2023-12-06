import qualified Shared

main :: IO ()
main = print . solve . lines =<< readFile "data.txt"

solve :: [String] -> Integer
solve [times, distances] = 
  product
  $ toInteger
  . uncurry Shared.calculate
  <$> zip (parseLine 6 times) (parseLine 10 distances) 

parseLine :: Int -> String -> [Integer]
parseLine n line = map read . words $ drop n line
