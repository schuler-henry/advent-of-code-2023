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

-----#ooooo#
-----o-----o
--#oooooo#-o
--o--o---o-o
--o--#ooo#-o
--#oooooooo#


solution :: IO ()
solution = do
  fileContent <- readFile "input.txt"
  -- For validation we need to process 10 pairs, for input we need to process 1000 pairs
  putStrLn $ show $ maximum $ map calculateArea $ getCornerPairs $ parseFile $ fileContent
