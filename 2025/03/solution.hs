import Data.Char
import System.IO

-- 1. Find largest digit (except last index)
-- 2. From that index, find largest digit to its right.
type Index = Int
type ValueWithIndex = (Int, Index) -- (value, index)

rowToIntList :: String -> [Int]
rowToIntList = map (read . pure :: Char -> Int)

appendIndex :: [Int] -> [ValueWithIndex]
appendIndex xs = zip xs [0..(length xs - 1)]

getLargestNumBeforeLast :: [ValueWithIndex] -> ValueWithIndex
getLargestNumBeforeLast xs = foldr (\(value, val_index) acc -> if value >= fst acc then (value, val_index) else acc) (-1, -1) (init xs)

getLargestNumAfter :: ValueWithIndex -> [ValueWithIndex] -> ValueWithIndex
getLargestNumAfter (start_val, start_idx) xs = foldr (\(value, val_index) acc -> if value >= fst acc then (value, val_index) else acc) (-1, -1) (drop (start_idx + 1) xs)

getLargestNumBetween :: Index -> Index -> [ValueWithIndex] -> ValueWithIndex
getLargestNumBetween start_idx end_idx xs = foldr (\(value, val_index) acc -> if value >= fst acc then (value, val_index) else acc) (-1, -1) (drop (start_idx + 1) $ take (end_idx) xs)

getJoltage :: [ValueWithIndex] -> Int
getJoltage xs = (\(fst_val, fst_idx) -> fst_val * 10 + fst (getLargestNumAfter (fst_val, fst_idx) xs)) (getLargestNumBeforeLast xs)

getSecondJoltageValues :: [ValueWithIndex] -> [ValueWithIndex]
getSecondJoltageValues xs = foldr (\joltage_idx acc -> getLargestNumBetween (snd (if length acc <= 0 then (-1,-1) else head acc)) (length xs - joltage_idx) xs : acc) [] [0..11]

secondJoltageValuesToJoltage :: [ValueWithIndex] -> Int
secondJoltageValuesToJoltage xs = foldr (\((value, _), factor) acc -> acc + value * 10 ^ factor) 0 (zip xs [0..])

solution :: IO ()
solution = do
  fileContent <- readFile "input.txt"
  putStrLn $ show $ sum $ getJoltage . appendIndex . rowToIntList <$> lines fileContent
  putStrLn $ show $ sum $ secondJoltageValuesToJoltage . getSecondJoltageValues . appendIndex . rowToIntList <$> lines fileContent