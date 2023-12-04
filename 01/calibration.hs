import Data.Char
import System.IO

-- 1)
listToInt :: [Int] -> Int
listToInt = snd . foldr (\v (m, a) -> (m * 10, v * m + a)) (1, 0)

getLineValue :: String -> Int
getLineValue input = listToInt $ digitToInt <$> ([head, last] <*> [(filter isDigit input)])

calibrationOne :: IO ()
calibrationOne = do
  filecontent <- readFile "input.txt"
  putStrLn $ show $ foldr (+) 0 $ getLineValue <$> (lines filecontent)

-- Solution: 55386

-- 2)
extractDigits :: String -> [Int]
extractDigits [] = []
extractDigits (x:xs) = if isDigit x then digitToInt x : extractDigits xs else
                          case x : take 1 xs of
                            "on" -> if x : take 2 xs == "one" then 1 : extractDigits xs else extractDigits xs
                            "tw" -> if x : take 2 xs == "two" then 2 : extractDigits xs else extractDigits xs
                            "th" -> if x : take 4 xs == "three" then 3 : extractDigits xs else extractDigits xs
                            "fo" -> if x : take 3 xs == "four" then 4 : extractDigits xs else extractDigits xs
                            "fi" -> if x : take 3 xs == "five" then 5 : extractDigits xs else extractDigits xs
                            "si" -> if x : take 2 xs == "six" then 6 : extractDigits xs else extractDigits xs
                            "se" -> if x : take 4 xs == "seven" then 7 : extractDigits xs else extractDigits xs
                            "ei" -> if x : take 4 xs == "eight" then 8 : extractDigits xs else extractDigits xs
                            "ni" -> if x : take 3 xs == "nine" then 9 : extractDigits xs else extractDigits xs
                            otherwise -> extractDigits xs

getLineValue' :: String -> Int
getLineValue' input = listToInt $ [head, last] <*> [extractDigits input]

calibrationTwo :: IO ()
calibrationTwo = do
  filecontent <- readFile "input.txt"
  putStrLn $ show $ foldr (+) 0 $ getLineValue' <$> (lines filecontent)

-- Solution: 54824