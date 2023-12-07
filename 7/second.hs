import Data.List (elemIndex)
import Data.Map (Map)
import qualified Data.Map as Map
import Debug.Trace
import Data.List (sort, sortBy)
import qualified Shared
import Shared (Player (..), Type (..))

main :: IO ()
main = print . solve . lines =<< readFile "data.txt"


compareHands :: String -> String -> Ordering
compareHands [] [] = EQ
compareHands (x:xs) (y:ys)
  | cardValue x == cardValue y = compareHands xs ys
  | otherwise  = cardValue x `compare` cardValue y 

cardValue :: Char -> Int
cardValue c =
  maybe 0 succ $ elemIndex c ['J', '2', '3', '4', '5', '6', '7', '8', '9', 'T', 'J', 'Q', 'K', 'A']

solve :: [String] -> Integer
solve lines = Shared.joinBids 1 0 . sort $ Shared.toPlayer . Shared.parseLine <$> lines

getType :: String -> Type
getType hand = Shared.getType hand . jokerize $ Shared.getOccurences hand

jokerize :: Map Char Int -> Map Char Int
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

headMaybe :: [a] -> Maybe a
headMaybe [] = Nothing
headMaybe (x:xs) = Just x

instance Eq Player where
  (Player ah _) == (Player bh _) = 
    getType ah == getType bh && ah == bh

instance Ord Player where
  compare (Player ah _) (Player bh _) =
    if c == EQ
      then compareHands ah bh 
      else c
    where c = getType ah `compare` getType bh
