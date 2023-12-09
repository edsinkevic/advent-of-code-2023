import Data.Map (Map)
import qualified Data.Map as Map
import Control.Monad (unless)
import Debug.Trace

type InstrSet = Map String (String, String)

main :: IO ()
main = print . solve . lines =<< readFile "data.txt"


solve :: [String] -> Integer
solve (instructions:_:rest) = 
  toInteger . length . computeSequence instructions $ parseMap Map.empty rest

computeSequence :: String -> InstrSet -> String
computeSequence instructions instrSet =
  computeSequence' instructions instructions "AAA" instrSet []

computeSequence' :: String -> String -> String -> InstrSet -> String -> String
computeSequence' instructions [] currentKey instrSet solution =
  computeSequence' instructions instructions currentKey instrSet solution

computeSequence' _ _ "ZZZ" _ solution = solution

computeSequence' instructions (i:is) currentKey instrSet solution =
  let nextKey = 
        traceShowId . pickSide i <$> Map.lookup currentKey instrSet
      nextAction key = 
        computeSequence' instructions is key instrSet (i:solution) in
    maybe solution nextAction nextKey
      
pickSide :: Char -> (String, String) -> String
pickSide side (l, r) = if (traceShowId side) == 'R' then r else l
  

parseMap :: InstrSet -> [String] -> InstrSet
parseMap pmap [] = pmap
parseMap pmap (l:ls) = 
  let key = takeWhile (/= ' ') l
      afterKey = tail $ dropWhile (/= '(') l
      left = takeWhile (/= ',') afterKey
      afterLeft = tail $ dropWhile (/= ' ') afterKey
      right = takeWhile (/= ')') afterLeft
      newMap = Map.insert key (left, right) pmap in
        parseMap newMap ls
