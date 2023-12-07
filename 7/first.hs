import Data.List (elemIndex)
import Data.Map (Map)
import qualified Data.Map as Map
import Debug.Trace
import Data.List (sort)

import qualified Shared
import Shared (Type (..), Player (..))

main :: IO ()
main = print . solve . lines =<< readFile "data.txt"

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
  maybe 0 succ $ elemIndex c ['2', '3', '4', '5', '6', '7', '8', '9', 'T', 'J', 'Q', 'K', 'A']

solve :: [String] -> Integer
solve lines = 
  Shared.joinBids 1 0
  . sort
  $ Shared.toPlayer
  . Shared.parseLine
  <$> lines

getType :: String -> Type
getType hand
  | oLength == handLength = High
  | oLength == handLength - 1 = One
  | oLength == handLength - 2 && highestOccurence == 3 = Three
  | oLength == handLength - 2 = Two
  | oLength == handLength - 3 && highestOccurence == 3 = Full
  | highestOccurence == 4 = Four
  | oLength == 1 = Five
  | otherwise = error "Hand had no type"
    where occurences = Shared.getOccurences hand
          oLength = length occurences
          handLength = length hand
          highestOccurence = maximum $ Map.elems occurences
