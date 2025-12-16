import Data.Char
import System.IO
import Data.Set (toList, fromList)

type Range = (Int, Int)

splitOnEmptyLine :: String -> ([String], [String])
splitOnEmptyLine input = break (== "") (lines input)

splitOn :: Char -> String -> [String]
splitOn separator = foldr (\char (x:xs) -> if char == separator then "" : x : xs else (char : x) : xs ) [""]

parseRanges :: [String] -> [Range]
parseRanges input = foldr (\stringRange acc -> (\(left:right) -> (read left, read $ head right) : acc) (splitOn '-' stringRange)) [] input

parseValues :: [String] -> [Int]
parseValues = foldr (\val acc -> (read::String->Int) val : acc) [] 

parseInput :: String -> ([Range], [Int])
parseInput input = (\(ranges, values) -> (parseRanges ranges,parseValues $ drop 1 values)) (splitOnEmptyLine input)

freshIngredients :: ([Range], [Int]) -> [Int]
freshIngredients (fresh_ids, ingredients) = filter (\id -> any (\range -> id >= fst range && id <= snd range) fresh_ids) ingredients

numOfFreshIds :: ([Range], [Int]) -> Int
numOfFreshIds (fresh_ids, _) = foldr (\(low, high) acc -> acc + high-low+1) 0 (reduceToUniqueIds fresh_ids)

reduceToUniqueIds :: [Range] -> [Range]
reduceToUniqueIds [] = []
reduceToUniqueIds (x:xs) = x : reduceToUniqueIds (foldr (\range acc -> removeRange range x ++ acc) [] xs)

-- range to update; the range to remove
removeRange :: Range -> Range -> [Range]
removeRange (low1, high1) (low2, high2)
  | high2 < low1 || low2 > high1 = [(low1, high1)]
  | low2 <= low1 && high2 >= high1 = []
  | low2 > low1 && high2 < high1 = [(low1, low2 - 1), (high2 + 1, high1)]
  | low2 <= low1 = [(high2 + 1, high1)]
  | high2 >= high1 = [(low1, low2 - 1)]
  | otherwise = [(low1, high1)]  -- Should not reach here

solution :: IO ()
solution = do
  fileContent <- readFile "input.txt"
  putStrLn $ show $ length $ freshIngredients $ parseInput fileContent
  putStrLn $ show $ numOfFreshIds $ parseInput fileContent