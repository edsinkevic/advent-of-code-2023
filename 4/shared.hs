module Shared where
import Data.Set (Set, intersection, fromList)

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
