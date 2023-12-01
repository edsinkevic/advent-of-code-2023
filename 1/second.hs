main :: IO ()
main = do
  content <- readFile "data1.txt" 
  let result = foldl accumulator 0 $ lines content
  _ <- print result 
  return ()

accumulator :: Integer -> String -> Integer
accumulator x y = x + calibration y

calibration :: String -> Integer
calibration = firstAndLastToDigit . digitalize

firstAndLastToDigit :: String -> Integer
firstAndLastToDigit x = read [head x, last x]

deduceDigitFromStart :: String -> String
deduceDigitFromStart input = case input of
  ('o':'n':'e':xs) -> "1"
  ('t':'w':'o':xs) -> "2"
  ('t':'h':'r':'e':'e':xs) -> "3"
  ('f':'o':'u':'r':xs) -> "4"
  ('f':'i':'v':'e':xs) -> "5"
  ('s':'i':'x':xs) -> "6"
  ('s':'e':'v':'e':'n':xs) -> "7"
  ('e':'i':'g':'h':'t':xs) -> "8"
  ('n':'i':'n':'e':xs) -> "9"
  x:xs -> if isNumber x then x:[] else ""
  [] -> ""

digitalize :: String -> String
digitalize [] = []
digitalize input = 
  deduceDigitFromStart input <> (digitalize $ tail input)

isNumber :: Char -> Bool
isNumber c = c >= '1' && c <= '9'
