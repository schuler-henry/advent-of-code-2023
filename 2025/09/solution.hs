import Data.Char
import System.IO

type Coord = (Int, Int)
type Pair = (Coord, Coord)

parseFile :: String -> [Coord]
parseFile content = map parseLine (lines content)
  where parseLine line = let [x, y] = map read (splitAtChar ',' line) in (x, y)
        splitAtChar :: Char -> String -> [String]
        splitAtChar c str = foldr (\ch (acc:rest) -> if ch == c then "":acc:rest else (ch:acc):rest) [""] str

getCornerPairs :: [Coord] -> [Pair]
getCornerPairs coords = [(c1,c2) | (i,c1) <- zip [0..] coords, (j, c2) <- zip [0..] coords, i < j]

calculateArea :: Pair -> Int
calculateArea ((x1, y1), (x2, y2)) = (abs (x2 - x1) + 1) * (abs (y2 - y1) + 1)

-- Part 2

-- ---#ooooo#
-- ---o-----o
-- #oooooo#-o
-- o--o---o-o
-- o--#ooo#-o
-- #oooooooo#

range :: Int -> Int -> [Int]
range a b = if a <= b then [a..b] else [b..a]

drawLines :: [Coord] -> ([Coord], [Coord])
drawLines coords = (coords, snd $ calculateLines ((last coords):coords, []))
  where 
    calculateLines :: ([Coord], [Coord]) -> ([Coord], [Coord])
    calculateLines ([p], linePoints) = ([], linePoints)
    calculateLines ((p1:p2:ps), linePoints) = calculateLines (p2:ps, linePoints ++ [(x, y) | x <- (range (fst p1) (fst p2)), y <- (range (snd p1) (snd p2)), not (x == fst p1 && y == snd p1), not (x == fst p2 && y == snd p2)]) 

getBoundingBox :: [Coord] -> (Int, Int, Int, Int)
getBoundingBox [] = error "No coordinates provided"
getBoundingBox (c:coords) = foldr (\(x,y) (minX, maxX, minY, maxY) -> (min minX x, max maxX x, min minY y, max maxY y)) (fst c, fst c, snd c, snd c) coords

-- evalArea :: ([Coord], [Coord]) -> ([Coord], [Coord])
-- evalArea (cornerPoints, linePoints) = (cornerPoints, linePoints ++ calcArea)
--   where
--     let (minX, maxX, minY, maxY) = getBoundingBox cornerPoints
--     calcArea = foldr (\x accX -> accX ++ fst $ foldr (\y (accY, state) -> processCoordinate x y (accY, state)) ([], 0) [minY .. maxY]) [] [minX .. maxX]
--     isInside x y (acc, 0) = if elem (x,y) cornerPoints then (acc, 1)
--                             else if elem (x,y) linePoints then (acc,2)
--     isInside x y (acc, 2) = if elem (x,y) cornerPoints then (acc, 3)
    -- isInside  

-- drawArea :: ([Coord], [Coord]) -> String
-- drawArea (cornerPoints, linePoints) = foldr (\y filed -> filed ++ foldr (\x row -> if elem (x,y) cornerPoints then '#':row else if elem (x,y) linePoints then 'o':row else ' ':row) "\n" [minX .. maxX]) "" [minY .. maxY]
--   where
--     (minX, maxX, minY, maxY) = getBoundingBox cornerPoints

cornerPointsInside ()

solution :: IO ()
solution = do
  fileContent <- readFile "input.txt"
  putStrLn $ show $ maximum $ map calculateArea $ getCornerPairs $ parseFile $ fileContent
  writeFile "test.txt" $ drawArea $ drawLines $ parseFile $ fileContent
