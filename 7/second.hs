import Data.List (elemIndex)
import Data.Map (Map)
import qualified Data.Map as Map
import Debug.Trace
import Data.List (sort, sortBy)

main :: IO ()
main = print . solve . lines =<< readFile "data.txt"

data Type = High | One | Two | Three | Full | Four | Five deriving (Enum, Eq, Ord, Show)

data Player = Player String Integer deriving Show

instance Eq Player where
  (Player ah _) == (Player bh _) = 
    getType ah == getType bh && ah == bh

instance Ord Player where
  compare (Player ah _) (Player bh _) =
    if c == EQ
      then compareHands ah bh 
      else c
    where c = getType ah `compare` getType bh

compareHands :: String -> String -> Ordering
compareHands [] [] = EQ
compareHands (x:xs) (y:ys)
  | cardValue x == cardValue y = compareHands xs ys
  | otherwise  = cardValue x `compare` cardValue y 

cardValue :: Char -> Int
cardValue c =
  maybe 0 succ $ elemIndex c ['J', '2', '3', '4', '5', '6', '7', '8', '9', 'T', 'J', 'Q', 'K', 'A']


solve :: [String] -> Integer
solve lines = joinBids 1 0 . sort $ toPlayer . parseLine <$> lines

joinBids :: Integer -> Integer -> [Player] -> Integer
joinBids _ sum [] = sum
joinBids multiplier sum ((Player _ bid):xs) =
  joinBids (succ multiplier) (sum + bid * multiplier) xs

toPlayer :: (String, Integer) -> Player
toPlayer (hand, bid) = Player hand bid

getType :: String -> Type
getType hand
  | oLength == 1 = Five
  | highestOccurence == 4 = Four
  | oLength == handLength = High
  | oLength == handLength - 1 = One
  | oLength == handLength - 2 && highestOccurence == 3 = Three
  | oLength == handLength - 2 = Two
  | oLength == handLength - 3 && highestOccurence == 3 = Full
  | otherwise = error $ show hand
    where occurences = traceShowId . jokerize $ getOccurences hand
          handLength = length hand
          oLength = length occurences 
          highestOccurence = maximum $ Map.elems occurences

jokerize occurences = 
  let jokers = Map.findWithDefault 0 'J' occurences
      withoutJokers = Map.delete 'J' occurences
      highestKey = 
        maybe 'A' id
        . headMaybe
        . map fst
        . sortBy (\x y -> snd y `compare` snd x)
        $ Map.assocs withoutJokers in 
      Map.insertWith (+) highestKey jokers withoutJokers

headMaybe [] = Nothing
headMaybe (x:xs) = Just x

getOccurences :: String -> Map Char Int
getOccurences = foldl accumulate Map.empty
  where accumulate accum next = Map.insertWith (+) next 1 accum

parseLine :: String -> (String, Integer)
parseLine line =
  let [hand, bid] = words line in
    (hand, read bid)
