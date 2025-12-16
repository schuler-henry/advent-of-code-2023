import Data.Char
import System.IO

type Coord = (Int, Int)

fieldToPaperCoords :: String -> [Coord]
fieldToPaperCoords field = concat $ foldr (\(line, y) acc -> foldr (\(char, x) line_acc -> if char == '@' then (x,y) : line_acc else line_acc) [] (zip line [0..]) : acc) [] (zip (lines field) [0..])

paperCoordsAround :: Coord -> [Coord] -> [Coord]
paperCoordsAround target coords = concat $ foldr (\x_diff x_acc -> foldr (\y_diff y_acc -> if elem (fst target + x_diff, snd target + y_diff) coords then (fst target + x_diff, snd target + y_diff) : y_acc else y_acc) [] [-1,0,1] : x_acc) [] [-1,0,1]

allCoordsWithLessThenFourAround :: [Coord] -> [Coord]
allCoordsWithLessThenFourAround coords = foldr (\target target_acc -> if length (paperCoordsAround target coords) < 5 then target : target_acc else target_acc) [] coords

removeCoords :: [Coord] -> [Coord] -> [Coord]
removeCoords toRemove coords = filter (\v -> not $ elem v toRemove) coords

applyUntilRemoved :: [Coord] -> [Coord]
applyUntilRemoved coords = (\result -> if length result > 0 then result ++ (applyUntilRemoved (removeCoords result coords)) else result) (allCoordsWithLessThenFourAround coords)

solution :: IO ()
solution = do
  fileContent <- readFile "input.txt"
  putStrLn $ show $ length $ allCoordsWithLessThenFourAround $ fieldToPaperCoords fileContent
  putStrLn $ show $ length $ applyUntilRemoved $ fieldToPaperCoords fileContent