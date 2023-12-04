import Data.Set (Set, intersection, fromList)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Char (isDigit)
import qualified Shared as Shared


main :: IO ()
main = print =<< solve <$> readFile "data.txt"

solve :: String -> Integer
solve content = 
  Map.foldr (+) 0
  . foldl accumulatePoints Map.empty
  $ lines content

type Points = Map Integer Integer

accumulatePoints :: Points -> String -> Points
accumulatePoints points line = 
  let (cardNumber, afterColon) = parseCardNumber line
      cardPoints = calculatePoints afterColon
      updated = Map.insertWith (+) cardNumber 1 points
      copies = Map.findWithDefault 1 cardNumber updated in
    addCopies cardNumber cardPoints copies updated

addCopies :: Integer -> Integer -> Integer -> Points -> Points
addCopies currentCard cardPoints toAdd points = 
  case cardPoints of
    0 -> points
    _ -> 
      addCopies currentCard (cardPoints - 1) toAdd
      $ Map.insertWith (+) (currentCard + cardPoints) toAdd points
    
calculatePoints :: String -> Integer
calculatePoints line = 
  toInteger
  . length 
  . uncurry intersection
  $ Shared.parseNumbers line

parseCardNumber :: String -> (Integer, String)
parseCardNumber line =
  (
    read . takeWhile isDigit $ dropWhile (not . isDigit) line,
    Shared.skipToColon line
  )
