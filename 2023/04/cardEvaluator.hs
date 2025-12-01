import Data.Char
import System.IO

-- This function returns the third element of a tripple
-- (a, b, c): tripple
-- returns:   last element of tripple
getList :: (a, b, c) -> c
getList (_, _, l) = l

-- This function parses a Card String input to a tripple containing the parsed numbers
-- input:   (Card) string to parse (Layout: "<anything> : <numbers 1> | <numbers 2>")
-- returns: A tripple containing the parsed numbers (Layout: (<undefined>, [<undefined>], [<numbers 1>, <numbers 2>]))
getLineElements :: String -> (Int, [Int], [[Int]])
getLineElements input = foldr (\v (m, ac, list) -> if v == '|' || v == ':' then
  (1, [], ac : list) else 
  if not $ isDigit v then 
  (1, ac, list) else 
  (m * 10, if m == 1 then 
    ((digitToInt v * m) : ac) else 
    ((head ac + (digitToInt v * m)) : (drop 1 ac)), list)) (1, [], []) input

-- This function computes the amount of winning numbers you have
-- l:       [[all winning numbers], [your numbers]]
-- returns: the amount of winning numbers I have
matchingNumbers :: [[Int]] -> Int
matchingNumbers l = foldr (\v a -> if elem v (last l) then a + 1 else a) 0 (head l)

-- This function translates the amount of winning numbers into points for task 1
-- x:       amount of winning numbers
-- returns: corresponding points 
points :: Int -> Int
points x = if x == 0 then 0 else (^) 2 (x - 1) where

-- This function combines the functions above and parses a Card string to the corresponding points
-- input:   Card String
-- returns: corresponding points
parseLine :: String -> Int
parseLine input = points $ matchingNumbers $ getList $ getLineElements input

-- This function reads the "input.txt" file and calculates the total points by first applying the parseLine function on every line and then building the sum
-- returns: Total points for task 1 (21568)
cardEvaluationOne :: IO ()
cardEvaluationOne = do
  filecontent <- readFile "input.txt"
  putStrLn $ show $ foldr (+) 0 $ parseLine <$> (lines filecontent)


-- 2)


-- This function maps a list of integers (here: used with matchingNumbers) with their index
-- numbers: list of matching numbers per line
-- returns: list of tuples of matching number and corresponding line index
zipMatchingNumbersWithIndex :: [Int] -> [(Int, Int)]
zipMatchingNumbersWithIndex numbers = zip [x | x <- [0 .. length numbers]] numbers

-- This function creates an array of a given size and initializes it with 1
-- size:    array size
-- returns: array with size times 1
initCards :: Int -> [Int]
initCards size = [1 | _ <- [1 .. size]]

-- This function creates an array containing the received copies from one card number (copies past the end of the available cards are discarded)
-- currentCardIndex:  index of the card that is evaluated
-- amount:            amount of cards with this index
-- matchingNumbers:   the amount of matching numbers for this card
-- numOfCards:        total amount of cards in the game
-- returns:           list of copies created by the current card
newCards :: Int -> Int -> Int -> Int -> [Int]
newCards currentCardIndex amount matchingNumbers numOfCards = take numOfCards $ [0 | _ <- [0 .. currentCardIndex]] ++ [amount | _ <- [1 .. matchingNumbers]] ++ [0 | _ <- [1 + matchingNumbers + (currentCardIndex + 1) .. numOfCards]]


-- This function combines the above functions and calculates a list with the amount of each card based on the matching numbers distribution
-- matchingNumbersDistribution: list of matching numbers for each card (line)
-- returns:                     amount of cards for each id
calculateFinalCardArray :: [Int] -> [Int]
calculateFinalCardArray matchingNumbersDistribution = foldr (\(i, wins) cards -> zipWith (+) cards $ newCards i (cards !! i) wins (length matchingNumbersDistribution)) (initCards $ length matchingNumbersDistribution) (reverse $ zipMatchingNumbersWithIndex matchingNumbersDistribution)

-- This function reads the "input.txt" file and calculates the total amount of cards by first calculating the matchingNumbers for each card (analog to task 1) and then calculating the resulting amount of cards for each card and finally building the sum
-- returns: Total amount of cards for task 2 (11827296)
cardEvaluationTwo :: IO ()
cardEvaluationTwo = do
  filecontent <- readFile "input.txt"
  putStrLn $ show $ foldr (+) 0 $ calculateFinalCardArray $ (matchingNumbers . getList . getLineElements) <$> (lines filecontent)
