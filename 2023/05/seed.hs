import Control.Monad.Reader
import Data.Char
import Data.List

-- Part 1
convertInputLine :: String -> [Int]
convertInputLine i = Prelude.map read $ tail $ words i

-- line: 39 0 15
readLine :: String -> (Int, Int, Int)
readLine i = let k = Prelude.map read $ words i in (k!!0, k!!1, k!!2)

-- line: 39 0 15
-- => 0-14 -> 39-53 (+14)
-- produce: (0, 14, (+ (39 - 0)))  ==  (min, max, transform_operation)
convertLine :: (Int, Int, Int) -> (Int, Int, Int -> Int)
convertLine (a, b, c) = (b, b + c - 1, (+(a - b)))

-- takes all lines containing step info and transforms them into a list of operations for each step
-- -> first dimension holds all the steps
-- -> second dimension holds all the operations per step
convertFile :: [String] -> [[(Int, Int, Int -> Int)]]
convertFile lines = fst $ Prelude.foldr (\line (result, acc) ->
    if Prelude.null line -- empty line means between steps -> clear acc, keep result
      then (result, [])
    else if isDigit $ head line -- line with content that needs to be parsed -> parse line and add to acc
      then (result, (convertLine $ readLine line) : acc)
    else -- line with text (step name) -> save acc to result and clear acc
      (acc : result, [])
  ) ([], []) lines

-- value: 12
-- xs: [(0, 14, (+39))]
-- result: (+39) 12 = 51
step :: Int -> [(Int, Int, Int -> Int)] -> Int
step value operations = if Prelude.null applicableOperations 
                          then value 
                        else operation value
                          where applicableOperations = Prelude.filter (\(min, max, _) -> value >= min && value <= max) operations
                                ((_, _, operation):_) = applicableOperations


solution :: IO ()
solution = do
  filecontent <- readFile "input.txt" -- read the file
  putStrLn $ show $ minimum $ Prelude.foldr (\inputValue resultArray -> -- iterate over each input value; applies all steps to all input values and returns the end result of each input value as a list
      (Data.List.foldl' (\acc stepOperations -> -- iterate over each operation (since they were built from bottom to top, a foldl is required to process the steps in the correct order)
                          step acc stepOperations -- apply the step to the current acc (whis is initially the input value) and return the value for the next step
                        ) inputValue (convertFile $ tail $ lines filecontent)) : resultArray -- get all operation_stages from the file and pass it to foldl + append result of foldl (input with all steps applied) to result array
    ) [] (convertInputLine $ head $ lines filecontent) -- gets input values and passes it to foldr


-- Part 2
-- Basically only the output of convertInputLine has to change

-- adaptInput :: [Int] -> [Int]
-- adaptInput [] = []
-- adaptInput (a:b:xs) = [a .. (a + b - 1)] ++ adaptInput xs

-- solution2 :: IO ()
-- solution2 = do
--   filecontent <- readFile "input.txt" -- read the file
--   putStrLn $ show $ minimum $ Prelude.foldr (\inputValue resultArray -> -- iterate over each input value; applies all steps to all input values and returns the end result of each input value as a list
--       (Data.List.foldl' (\acc stepOperations -> -- iterate over each operation (since they were built from bottom to top, a foldl is required to process the steps in the correct order)
--                           step acc stepOperations -- apply the step to the current acc (whis is initially the input value) and return the value for the next step
--                         ) inputValue (convertFile $ tail $ lines filecontent)) : resultArray -- get all operation_stages from the file and pass it to foldl + append result of foldl (input with all steps applied) to result array
--     ) [] (adaptInput $ convertInputLine $ head $ lines filecontent) -- gets input values and passes it to foldr

-- PROBLEM: Too many inputs -> takes too long
-- Idea: Do not create all numbers (too many) but work with intervals instead

-- Manual solution: building from back to front starting with lowest interval (since min number is asked at the end)
-- INPUT
--                                                                                                                                                        264807001 -  325363153
--                                                                                                                                                        444541979 -  796321208
--                                                                                                                                                       1076140984 - 1181043435
--                                                                                                                                                       1186343315 - 1252369370
--                                                                                                                                                       1381149926 - 1392529367
--                                                                                                                                                       1904897211 - 2400170751
--                                                                                                                                                       3136945476 - 3646674432
--                                                                                                                                                       3676523418 - 3720664300
--                                                                                                                                                       3895155702 - 4006236397
--                                                                                                                                                       4060485949 - 4250787494
-- INTERVAL BUILDUP
--                                                                                     fertilizer to water        soil to fertilizer          seed to soil
-- 0 - 129748072 => 737002290 - 866750362
--                  737002290 - 821406968 =>  215691496 -  300096174
--                                            215691496 -  293218004 => 1426436985 - 1503963493
--                                                                      1426436985 - 1503963493 =>  197458167 -  274984675
--                                                                                                  197458167 -  266781854 => 2744827122 - 2814150809
--                                                                                                                            2744827122 - 2814150809 => 2998681356 - 3068005043
--                                                                                                  266781855 -  274984675 => 3027323979 - 3035526799
--                                                                                                                            3027323979 - 3035526799 => 3281178213 - 3289381033  ==>> 3281178213 == 69323688
--                                            293218005 -  300096174 =>  301584645 -  308462814
--                                                                       301584645 -  308462814 =>  422784513 -  429662682
--                                                                                                  422784513 -  429662682 => 3183326637 - 3190204806
--                  821406969 - 854252811 =>  119263604 -  152109446
--                                            119263604 -  136410836 =>  849563940 -  866711172
--                                                                       849563940 -  866711172 => 1399532212 - 1416679444
--                                                                                                 1399532212 - 1416679444 =>  373513576 -  390660808
--                                            136410837 -  152109446 => 1347156326 - 1362854935
--                                                                      1347156326 - 1356562400 => 2103446221 - 2112852295
--                                                                                                 2103446221 - 2112852295 => 1910641996 - 1920048070
--                                                                      1356562401 - 1362854935 => 1036728223 - 1043020757
--                                                                                                 1036728223 - 1043020757 =>   10709587 -   17002121
--                  854252812 - 866750362 => 1457767969 - 1470265519
--                                           1457767969 - 1470265519 => 1829819369 - 1842316919
--                                                                      1829819369 - 1842316919 => 1747201471 - 1759699021
--                                                                                                 1747201471 - 1759699021 =>  721182835 -  733680385

-- SOLUTION: 69323688