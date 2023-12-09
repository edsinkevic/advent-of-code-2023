import qualified Shared

main :: IO ()
main = print . solve . lines =<< readFile "data.txt"

solve :: [String] -> Integer
solve lines = foldl accumulate 0 lines

accumulate :: Integer -> String -> Integer
accumulate sum line = sum + extrapolated (read <$> words line)

extrapolated :: [Integer] -> Integer
extrapolated history = extrapolate $ differences history [history]
  
extrapolate :: [[Integer]] -> Integer
extrapolate histories = foldl (flip (-)) 0 . reverse $ head <$> histories

differences :: [Integer] -> [[Integer]] -> [[Integer]]
differences history accum =
  if shouldEnd then reverse (diff:accum) else differences diff (diff:accum)
    where diff = Shared.difference history []
          shouldEnd = all (== 0) diff
  
