import Data.Char
import Data.Set (toList, fromList)
import System.IO

type Range = (Int, Int)

splitOn :: Char -> String -> [String]
splitOn separator = foldr (\char (x:xs) -> if char == separator then "" : x : xs else (char : x) : xs ) [""]

stringToRange :: String -> Range
stringToRange string = (\(left:right) -> (read left, read $ head right)) (splitOn '-' string)

parseInput :: [String] -> [Range]
parseInput = foldr (\stringRange acc -> stringToRange stringRange : acc) []

intToStringLength :: Int -> Int
intToStringLength = length . show

checkEqualBoundStringLength :: Range -> Bool
checkEqualBoundStringLength (low, high) = intToStringLength low == intToStringLength high

boundContainsNumber :: Range -> Int -> Bool
boundContainsNumber (low, high) number = number >= low && number <= high

getFirstStringHalfAsInt :: Int -> Int
getFirstStringHalfAsInt number = getFirstHalfAsInt (show number)

getFirstHalfAsInt :: String -> Int
getFirstHalfAsInt numberString = if length numberString == 1 then 0 else read $ take (div (length numberString) 2) numberString

getDuplicateSequence :: Int -> Int
getDuplicateSequence number = read (concat $ replicate 2 (show number))

getAllValuesInRange :: Range -> [Int]
getAllValuesInRange (low, high) = [low..high]

prependZerosToLength :: Int -> Int -> String
prependZerosToLength targetLength number = replicate (targetLength - intToStringLength number) '0' ++ show number

getSmallestEvenLength :: Range -> Int
getSmallestEvenLength (low, high) = 2 * (div (intToStringLength low) 2 + if even $ intToStringLength low then 0 else 1)

getLargestEvenLength :: Range -> Int
getLargestEvenLength (low, high) = 2 * div (intToStringLength high) 2

getPossibleValuesWithLengths :: Range -> Int -> [Int]
getPossibleValuesWithLengths (l,r) lowerBound = [(max (read ('1' : replicate (l - 1) '0')) lowerBound)..(0 + (read (replicate (r) '9')))]

findInvalidIdsInRange :: Range -> [Int]
findInvalidIdsInRange range = if checkEqualBoundStringLength range then
  (
    if even $ intToStringLength (fst range) then
      if getFirstStringHalfAsInt (fst range) == getFirstStringHalfAsInt (snd range) then
        if boundContainsNumber range (getDuplicateSequence (getFirstStringHalfAsInt (fst range))) then
          [getDuplicateSequence (getFirstStringHalfAsInt (fst range))]
        else
          []
      else
        foldr (\num acc -> if boundContainsNumber range (getDuplicateSequence num) then getDuplicateSequence num : acc else acc) [] (getAllValuesInRange (getFirstStringHalfAsInt (fst range), getFirstStringHalfAsInt (snd range)))
    else
      []
  )
  else
    foldr (\num acc -> if boundContainsNumber range (getDuplicateSequence num) then getDuplicateSequence num : acc else acc) [] (getPossibleValuesWithLengths (div (getSmallestEvenLength range) 2, div (getLargestEvenLength range) 2) (getFirstStringHalfAsInt $ fst range))

-- Check length of lower and upper bound
-- => if equal
  -- => if length uneven => no invalid ids possible
  -- => if length even
    -- => if first half is equal => check if second half (as duplicate) is in range
    -- => else: check for all combination between lower and upper bound of first half if duplicate is in range
-- => if not equal
  -- => 






-- 1 wise repeat: concat $ foldr (\repeatTimes acc -> (\val -> (concat $ replicate repeatTimes (show val))) <$> [1..9] : acc) [] [length l..length r]
-- 2 wise repeat 


-- min length + max length
-- ceil(min_string_length / num_len) - floor(max_string_length / num_len) => Number of repeats possible

numberOfPossibleRepeats :: Range -> Int -> Range
numberOfPossibleRepeats range numLen = (ceiling (fromIntegral (intToStringLength $ fst range) / fromIntegral numLen), floor (fromIntegral (intToStringLength $ snd range) / fromIntegral numLen))

-- if we can repeat at least once => lower bound = 2, then we can generate the numbers
smallestNumberWithLength :: Int -> Int
smallestNumberWithLength length = 1 * (10^(length - 1))

largestNumberWithLength :: Int -> Int
largestNumberWithLength length = (10^length) - 1

generateRepeatedNumbers :: Range -> Int -> [Int]
generateRepeatedNumbers (low, high) numLen = if high < 2 then [] else
  concat $ foldr (\value repeatedNumbers -> foldr (\repeatTimes acc -> (read $ concat $ replicate repeatTimes (show value)) : acc) [] [max 2 low..high] : repeatedNumbers) [] [smallestNumberWithLength numLen .. largestNumberWithLength numLen]

rangeToPossibleRepeatSequenceLength :: Range -> [Int]
rangeToPossibleRepeatSequenceLength (low, high) = [1..div (intToStringLength high) 2]

rangeToPossibleKeys :: Range -> [Int]
rangeToPossibleKeys (low, high) = concat $ foldr (\seqLength keys -> generateRepeatedNumbers (numberOfPossibleRepeats (low, high) seqLength) seqLength : keys) [] (rangeToPossibleRepeatSequenceLength (low, high))

filterKeysInRange :: Range -> [Int] -> [Int]
filterKeysInRange (low, high) keys = filter (\key -> key >= low && key <= high) keys

findAllKeysInRange :: Range -> [Int]
findAllKeysInRange range = toList $ fromList (filterKeysInRange range (rangeToPossibleKeys range))

solution :: IO ()
solution = do
  fileContent <- readFile "input.txt"
  putStrLn $ show $ sum $ foldr (\range acc -> acc ++ findInvalidIdsInRange range) [] (parseInput $ splitOn ',' fileContent)
  putStrLn $ show $ sum $ foldr (\range acc -> acc ++ findAllKeysInRange range) [] (parseInput $ splitOn ',' fileContent)