module Shared where
import Data.List (elemIndex)
import Data.Map (Map)
import qualified Data.Map as Map
import Debug.Trace
import Data.List (sort)

data Type = High | One | Two | Three | Full | Four | Five deriving (Enum, Eq, Ord, Show)
data Player = Player String Integer deriving Show

joinBids :: Integer -> Integer -> [Player] -> Integer
joinBids _ sum [] = sum
joinBids multiplier sum ((Player _ bid):xs) =
  joinBids (succ multiplier) (sum + bid * multiplier) xs

toPlayer :: (String, Integer) -> Player
toPlayer (hand, bid) = Player hand bid

getOccurences :: String -> Map Char Int
getOccurences = foldl accumulate Map.empty
  where accumulate accum next = Map.insertWith (+) next 1 accum

parseLine :: String -> (String, Integer)
parseLine line =
  let [hand, bid] = words line in
    (hand, read bid)

getType :: String -> Map Char Int -> Type
getType hand occurences
  | oLength == handLength = High
  | oLength == handLength - 1 = One
  | oLength == handLength - 2 && highestOccurence == 3 = Three
  | oLength == handLength - 2 = Two
  | oLength == handLength - 3 && highestOccurence == 3 = Full
  | highestOccurence == 4 = Four
  | oLength == 1 = Five
  | otherwise = error "Hand had no type"
    where oLength = length occurences
          handLength = length hand
          highestOccurence = maximum $ Map.elems occurences
