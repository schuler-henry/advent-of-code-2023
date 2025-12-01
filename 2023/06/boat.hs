-- The problem of today can be reduced to a quadratic formula problem since:
-- d = (t - z) * z    where d = distance, t = time, z = time for traveling (=> t-z = time for pressing the button == velocity)
-- t = 7
-- d = 9
-- => (7, 9)

type Time = Int
type Distance = Int

-- - z² + tz - d
-- (-t + sqr(t² - 4d)) / (-2)
quadraticFormulaOne :: RealFloat a => a -> a -> a -> a
quadraticFormulaOne a b c = ((-b) + sqrt ((b*b) - (4*a*c))) / (2*a)

quadraticFormulaTwo :: RealFloat a => a -> a -> a -> a
quadraticFormulaTwo a b c = ((-b) - sqrt ((b*b) - (4*a*c))) / (2*a)

quadraticFormula :: RealFloat a => (Time, Distance) -> (a, a)
quadraticFormula (time, distance) = (quadraticFormulaOne a b c, quadraticFormulaTwo a b c)
                                    where a = -1
                                          b = fromIntegral time
                                          c = -(fromIntegral distance)

winningCombinations :: RealFloat a => (a, a) -> Int
winningCombinations (l, r) | isNaN l || isNaN r = 0
                           | otherwise = floor r - ceiling l + 1

getAllWinningCombinations :: [(Time, Distance)] -> [Int]
getAllWinningCombinations list = winningCombinations . quadraticFormula <$> fmap (\(x, y) -> (x, y + 1)) list

-- File operations
parseLine :: String -> [Int]
parseLine a = read <$> tail (words a)

zipTimeDistance :: [Time] -> [Distance] -> [(Time, Distance)]
zipTimeDistance = zip


-- IO
taskOne :: IO ()
taskOne = do
  fileContent <- readFile "input.txt"
  putStrLn $ show $ product $ getAllWinningCombinations $ zipTimeDistance (parseLine $ head $ lines fileContent) (parseLine $ last $ lines fileContent)


-- TaskTwo (ignore spaces between numbers)
parseLineTwo :: String -> [Int]
parseLineTwo a = read <$> [concat $ tail (words a)]

taskTwo :: IO ()
taskTwo = do
  fileContent <- readFile "input.txt"
  putStrLn $ show $ product $ getAllWinningCombinations $ zipTimeDistance (parseLineTwo $ head $ lines fileContent) (parseLineTwo $ last $ lines fileContent)
