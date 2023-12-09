import qualified Shared

main :: IO ()
main = print . solve . lines =<< readFile "data.txt"

solve :: [String] -> Integer
solve lines = foldl accumulate 0 lines

accumulate :: Integer -> String -> Integer
accumulate sum line = sum + extrapolated (read <$> words line)

extrapolated :: [Integer] -> Integer
extrapolated history = differences history (head $ reverse history)

differences :: [Integer] -> Integer -> Integer
differences history accum =
  if shouldEnd then accum else differences diff (accum + diffLast)
    where diff = Shared.difference history []
          diffLast = head $ reverse diff
          shouldEnd = all (== 0) diff
