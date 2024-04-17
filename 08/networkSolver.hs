import Data.List
import Data.Maybe

type Position = String
type Dictionary = ([String], [(String, String)])
type Instruction = String

-- AAA = (BBB, CCC) => ("AAA", ("BBB", "CCC"))
parseLine :: String -> (String, (String, String))
parseLine input = (head elements, (tail $ init $ elements !! 2, init $ last elements))
                  where elements = words input

parseLines :: [String] -> Dictionary
parseLines = foldr (\line (l, r) -> 
    let (nl, nr) = parseLine line in (nl:l, nr:r)
  ) ([], [])

step :: Position -> Dictionary -> Instruction -> Position
step position (search, target) inst = case head inst of
  'L' -> l
  'R' -> r
  where (l,r) = target !! fromMaybe 0 (elemIndex position search)
        
steps :: (Position, Int) -> Position -> Dictionary -> Instruction -> (Position, Int)
steps (startPosition, stepsNumber) endPosition dictionary instruction | startPosition == endPosition = (startPosition, stepsNumber)
                                                                      | otherwise = steps (step startPosition dictionary instruction, stepsNumber + 1) endPosition dictionary (tail instruction ++ take 1 instruction)


-- IO
taskOne :: IO ()
taskOne = do
  fileContent <- readFile "input.txt"
  putStrLn $ show $ snd $ steps ("AAA", 0) "ZZZ" (parseLines $ filter (/="") $ tail $ lines fileContent) (head $ lines fileContent)