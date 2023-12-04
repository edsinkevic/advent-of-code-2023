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
  . foldl accumulateCopies Map.empty
  $ lines content

type Copies = Map Integer Integer

accumulateCopies :: Copies -> String -> Copies
accumulateCopies copies line = 
  let (cardNumber, afterColon) = parseCardNumber line
      cardPoints = calculateCopies afterColon
      updated = Map.insertWith (+) cardNumber 1 copies
      cardCopies = Map.findWithDefault 1 cardNumber updated in
    addCopies cardNumber cardPoints cardCopies updated

addCopies :: Integer -> Integer -> Integer -> Copies -> Copies
addCopies currentCard cardPoints toAdd copies = 
  case cardPoints of
    0 -> copies
    _ -> 
      addCopies currentCard (cardPoints - 1) toAdd
      $ Map.insertWith (+) (currentCard + cardPoints) toAdd copies
    
calculateCopies :: String -> Integer
calculateCopies line = 
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
