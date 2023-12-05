import Debug.Trace (traceShowId)

data Conversion = Conversion Integer Integer Integer
type PuzzleMap = [Conversion]

instance Show Conversion where
  show (Conversion a b c) = show ("Conversion " ++ show a ++ " " ++ show b ++ " " ++ show c)
  
main :: IO ()
main = print . solve . lines =<< readFile "testdata.txt"

solve :: [String] -> Integer
solve (seedLine:restLines) = 
  findMinLocation (parseSeeds seedLine)
  . reverse
  $ parseMaps [] restLines

findMinLocation :: [Integer] -> [PuzzleMap] -> Integer
findMinLocation seeds puzzleMaps = 
  case puzzleMaps of
    [] -> foldl1 min seeds
    (puzzleMap:rest) -> 
      findMinLocation (getMinLocation puzzleMap <$> seeds) rest
    
parseSeeds :: String -> [Integer]
parseSeeds line = read <$> words (drop 7 line)

parseMaps :: [PuzzleMap] -> [String] -> [PuzzleMap]
parseMaps accumulator (_:lines) = 
  let (puzzleMap, rest) = parseMap lines in
    case rest of
      [] -> puzzleMap:accumulator
      _ -> parseMaps (puzzleMap:accumulator) rest
      
parseMap :: [String] -> (PuzzleMap, [String])
parseMap (_:lines) = 
  (
    toConversion <$> takeWhile (not . null) lines,
    dropWhile (not . null) lines
  )

toConversion :: String -> Conversion
toConversion line = 
  let [destination, source, range] = read <$> words line in
    Conversion destination source range

getLocation :: Integer -> Conversion -> Maybe Integer
getLocation seed (Conversion s d r) =
  let x = seed - d in if x >= 0 && x <= r then Just $ s + x else Nothing

getMinLocation :: PuzzleMap -> Integer -> Integer
getMinLocation puzzleMap seed = 
  case puzzleMap of
    [] -> seed
    (first:rest) -> 
      maybe (getMinLocation rest seed) id $ getLocation seed first
