import qualified Shared

main :: IO ()
main = print . solve . lines =<< readFile "data.txt"

solve :: [String] -> Integer
solve lines = foldl accumulate 0 lines

accumulate :: Integer -> String -> Integer
accumulate sum line = sum + extrapolated (read <$> words line)

extrapolated :: [Integer] -> Integer
extrapolated history = extrapolate $ Shared.differences history [history]
  
extrapolate :: [[Integer]] -> Integer
extrapolate histories = foldl (+) 0 . reverse $ head . reverse <$> histories
