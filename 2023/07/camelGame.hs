import Data.List

type Hand = [Char]
type Bid = Int
type TypeArray = [Int]

-- SORT INTO TYPES
-- [5]
-- [4, 1]
-- [3, 2]
-- [3, 1, 1]
-- [2, 2, 1]
-- [2, 1, 1, 1]
-- [1, 1, 1, 1, 1]

calculateTypeArray :: Hand -> TypeArray
calculateTypeArray hand = sortBy (flip compare) $ init $ snd $ foldr (\char (lastChar, l@(x:xs)) -> if char == lastChar then (lastChar, x+1:xs) else (char, 1:l)) (' ', [0]) (sort hand)

getTypeIndex :: TypeArray -> Int
getTypeIndex (a:xs) | a == 5 = 6
                    | a == 4 = 5
                    | a == 3 = 2 + b
                    | a == 2 = b
                    | a == 1 = 0
                    | otherwise = 0
                    where (b:_) = xs

-- Example: categorizeType [("KTJJT", 1), ("KK677", 2), ("AAAAA", 3)]
categorizeType :: [(Hand, Bid)] -> ([(Hand, Bid)], [(Hand, Bid)], [(Hand, Bid)], [(Hand, Bid)], [(Hand, Bid)], [(Hand, Bid)], [(Hand, Bid)])
categorizeType = foldr (\v@(hand, _) (v6, v5, v4, v3, v2, v1, v0) ->
    case getTypeIndex (calculateTypeArray hand) of
      0 -> (v6, v5, v4, v3, v2, v1, v:v0)
      1 -> (v6, v5, v4, v3, v2, v:v1, v0)
      2 -> (v6, v5, v4, v3, v:v2, v1, v0)
      3 -> (v6, v5, v4, v:v3, v2, v1, v0)
      4 -> (v6, v5, v:v4, v3, v2, v1, v0)
      5 -> (v6, v:v5, v4, v3, v2, v1, v0)
      6 -> (v:v6, v5, v4, v3, v2, v1, v0)
  ) ([], [], [], [], [], [], [])


-- SORT TYPES BY STRENGTH
-- Strength: A, K, Q, J, T, 9, 8, 7, 6, 5, 4, 3, 2
-- [("KTJJT",1),("KK677",2)]

categorizeStrength :: [(Hand, Bid)] -> ([(Hand, Bid)], [(Hand, Bid)], [(Hand, Bid)], [(Hand, Bid)], [(Hand, Bid)], [(Hand, Bid)], [(Hand, Bid)], [(Hand, Bid)], [(Hand, Bid)], [(Hand, Bid)], [(Hand, Bid)], [(Hand, Bid)], [(Hand, Bid)])
categorizeStrength = foldr (\(hand, bid) (vA, vK, vQ, vJ, vT, v9, v8, v7, v6, v5, v4, v3, v2) ->
    case head hand of
      'A' -> ((tail hand, bid):vA, vK, vQ, vJ, vT, v9, v8, v7, v6, v5, v4, v3, v2)
      'K' -> (vA, (tail hand, bid):vK, vQ, vJ, vT, v9, v8, v7, v6, v5, v4, v3, v2)
      'Q' -> (vA, vK, (tail hand, bid):vQ, vJ, vT, v9, v8, v7, v6, v5, v4, v3, v2)
      'J' -> (vA, vK, vQ, (tail hand, bid):vJ, vT, v9, v8, v7, v6, v5, v4, v3, v2)
      'T' -> (vA, vK, vQ, vJ, (tail hand, bid):vT, v9, v8, v7, v6, v5, v4, v3, v2)
      '9' -> (vA, vK, vQ, vJ, vT, (tail hand, bid):v9, v8, v7, v6, v5, v4, v3, v2)
      '8' -> (vA, vK, vQ, vJ, vT, v9, (tail hand, bid):v8, v7, v6, v5, v4, v3, v2)
      '7' -> (vA, vK, vQ, vJ, vT, v9, v8, (tail hand, bid):v7, v6, v5, v4, v3, v2)
      '6' -> (vA, vK, vQ, vJ, vT, v9, v8, v7, (tail hand, bid):v6, v5, v4, v3, v2)
      '5' -> (vA, vK, vQ, vJ, vT, v9, v8, v7, v6, (tail hand, bid):v5, v4, v3, v2)
      '4' -> (vA, vK, vQ, vJ, vT, v9, v8, v7, v6, v5, (tail hand, bid):v4, v3, v2)
      '3' -> (vA, vK, vQ, vJ, vT, v9, v8, v7, v6, v5, v4, (tail hand, bid):v3, v2)
      '2' -> (vA, vK, vQ, vJ, vT, v9, v8, v7, v6, v5, v4, v3, (tail hand, bid):v2)
  ) ([], [], [], [], [], [], [], [], [], [], [], [], [])

sortStrength :: [(Hand, Bid)] -> [(Hand, Bid)]
sortStrength [] = []
sortStrength (x:[]) = [x]
sortStrength hands = concatMap sortStrength [vA, vK, vQ, vJ, vT, v9, v8, v7, v6, v5, v4, v3, v2]
                     where (vA, vK, vQ, vJ, vT, v9, v8, v7, v6, v5, v4, v3, v2) = categorizeStrength hands

sortTypes :: ([(Hand, Bid)], [(Hand, Bid)], [(Hand, Bid)], [(Hand, Bid)], [(Hand, Bid)], [(Hand, Bid)], [(Hand, Bid)]) -> [(Hand, Bid)]
sortTypes (v6, v5, v4, v3, v2, v1, v0) = concatMap sortStrength [v6, v5, v4, v3, v2, v1, v0]

-- Calculate task 1
calculateTaskOne :: [(Hand, Bid)] -> Int
calculateTaskOne hands = snd $ foldr (\(_, bid) (index, value) -> (index + 1, bid * index + value)) (1, 0) hands

-- IO
parseLine :: String -> (Hand, Bid)
parseLine line = (l, read r)
                 where (l:r:[]) = words line

parseFile :: [String] -> [(Hand, Bid)]
parseFile = map parseLine

taskOne :: IO ()
taskOne = do
  fileContent <- readFile "input.txt"
  putStrLn $ show $ calculateTaskOne $ sortTypes $ categorizeType $ parseFile $ lines fileContent


-- TASK 2 ADAPTIONS
getJokers :: Hand -> Int
getJokers hand = length $ elemIndices 'J' hand

removeJokers :: Hand -> Hand
removeJokers = filter (/='J')

calculateTypeArrayTwo :: Hand -> TypeArray
calculateTypeArrayTwo hand = s + getJokers hand : ss
                             where (s:ss) = sortBy (flip compare) $ snd $ foldr (\char (lastChar, l@(x:xs)) -> if char == lastChar then (lastChar, x+1:xs) else (char, 1:l)) (' ', [0]) (sort $ removeJokers hand)


categorizeTypeTwo :: [(Hand, Bid)] -> ([(Hand, Bid)], [(Hand, Bid)], [(Hand, Bid)], [(Hand, Bid)], [(Hand, Bid)], [(Hand, Bid)], [(Hand, Bid)])
categorizeTypeTwo = foldr (\v@(hand, _) (v6, v5, v4, v3, v2, v1, v0) ->
    case getTypeIndex (calculateTypeArrayTwo hand) of
      0 -> (v6, v5, v4, v3, v2, v1, v:v0)
      1 -> (v6, v5, v4, v3, v2, v:v1, v0)
      2 -> (v6, v5, v4, v3, v:v2, v1, v0)
      3 -> (v6, v5, v4, v:v3, v2, v1, v0)
      4 -> (v6, v5, v:v4, v3, v2, v1, v0)
      5 -> (v6, v:v5, v4, v3, v2, v1, v0)
      6 -> (v:v6, v5, v4, v3, v2, v1, v0)
  ) ([], [], [], [], [], [], [])

sortStrengthTwo :: [(Hand, Bid)] -> [(Hand, Bid)]
sortStrengthTwo [] = []
sortStrengthTwo (x:[]) = [x]
sortStrengthTwo hands = concatMap sortStrengthTwo [vA, vK, vQ, vT, v9, v8, v7, v6, v5, v4, v3, v2, vJ]
                        where (vA, vK, vQ, vJ, vT, v9, v8, v7, v6, v5, v4, v3, v2) = categorizeStrength hands

sortTypesTwo :: ([(Hand, Bid)], [(Hand, Bid)], [(Hand, Bid)], [(Hand, Bid)], [(Hand, Bid)], [(Hand, Bid)], [(Hand, Bid)]) -> [(Hand, Bid)]
sortTypesTwo (v6, v5, v4, v3, v2, v1, v0) = concatMap sortStrengthTwo [v6, v5, v4, v3, v2, v1, v0]

taskTwo :: IO ()
taskTwo = do
  fileContent <- readFile "input.txt"
  putStrLn $ show $ calculateTaskOne $ sortTypesTwo $ categorizeTypeTwo $ parseFile $ lines fileContent