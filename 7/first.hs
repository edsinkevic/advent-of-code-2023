import Data.List (elemIndex)
import Data.Map (Map)
import qualified Data.Map as Map
import Debug.Trace
import Data.List (sort)

import qualified Shared
import Shared (Type (..), Player (..))

main :: IO ()
main = print . solve . lines =<< readFile "data.txt"

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
getType hand = Shared.getType hand $ Shared.getOccurences hand

instance Eq Player where
  (Player ah _) == (Player bh _) = 
    getType ah == getType bh && ah == bh

instance Ord Player where
  compare (Player ah _) (Player bh _) =
    if c == EQ
      then compareHands ah bh 
      else c
    where c = getType ah `compare` getType bh
