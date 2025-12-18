import Data.Char
import System.IO
import Data.Set (toList, fromList)

type Coord = (Int, Int)
type Dimension = (Int, Int)
type Field = (Dimension, Coord, [Coord], [Coord])

-- Returns the start coord and all coords with splitters
parseFieldElements:: String -> (Dimension, Coord, [Coord])
parseFieldElements input = (\(start, splitters) -> ((length $ head $ lines input, length $ lines input), start, splitters)) $ foldr (\(y, line) (start, acc) -> (\(st, elem) -> if st == (0,0) then (start, concat [elem, acc]) else (st, concat [elem,acc])) $ foldr (\(x, c) (s, a) -> if c == 'S' then ((x, y), a) else if c == '^' then (s, (x,y):a) else (s,a)) ((0,0), []) $ zip [0..] line) ((0,0), []) $ zip [0..] (lines input)

parseField :: String -> Field
parseField input = let (dim, start, splitters) = parseFieldElements input in (dim, start, splitters, [])

getNextBeams :: Coord -> Field -> [Coord]
getNextBeams (x,y) (d, s, splitters, b) = if y + 1 == snd d then [] else if elem (x,y + 1) splitters then [(x-1,y+1), (x+1,y+1)] else [(x,y+1)]

appendUniqueBeams :: [Coord] -> [Coord] -> [Coord]
appendUniqueBeams newBeams existingBeams = foldr (\b acc -> if elem b acc then acc else b:acc) existingBeams newBeams

proceed :: [Coord] -> Field -> ([Coord], Field)
proceed lastPlacedBeams (d,s,splitters,beams) = (\newBeams -> (newBeams, (d,s,splitters,appendUniqueBeams newBeams beams))) $ foldr (\beam acc -> appendUniqueBeams (getNextBeams beam (d,s,splitters,beams)) acc) [] lastPlacedBeams

start :: Field -> ([Coord], Field)
start field@(d,s,_,_) = ([s], field)

applyAllSteps :: ([Coord], Field) -> ([Coord], Field)
applyAllSteps ([], field) = ([], field)
applyAllSteps (lastBeams, field) = applyAllSteps $ proceed lastBeams field

calculateNumberOfSplits :: Field -> Int
calculateNumberOfSplits (_,_,splitters,beams) = foldr (\(x,y) acc -> if elem (x,y-1) beams then acc + 1 else acc) 0 splitters

-- Part 2
proceed2 :: Coord -> Int -> Field -> [([Coord], Int)]
proceed2 lastPlacedBeam numBranches field@(d,s,sp,be) = (\nextCoords -> if length nextCoords == 0 then [([(-1,-1)], numBranches)] else map (\coord -> ([coord], numBranches)) nextCoords) $ getNextBeams lastPlacedBeam field

start2 :: Field -> ([([Coord], Int)], Field)
start2 field@(d,s,_,_) = ([([s], 1)], field)

-- Combines two list of (beam-pos, num-of-branches) into one, so that each beam position has only one entry with the 
-- total number of branches to reduce the number of entries that have to be processed
combineUnique2 :: [([Coord], Int)] -> [([Coord], Int)] -> [([Coord], Int)]
combineUnique2 uniqueL1 l2 = foldr (\coord acc -> newPair coord : acc) [] allCoords
  where allCoords = toList $ fromList $ concat [(map (head.fst) uniqueL1), (map (head.fst) l2)]
        newPair coord = foldr (\(_,num) (accC, accN) -> (accC, accN + num)) ([coord], 0) (concat [filterCoord coord uniqueL1, filterCoord coord l2])
        filterCoord coord l = filter (\((c:_), _) -> c == coord) l

-- Process the input row by row and always combine the beams going to the same coord
applyAllSteps2 :: ([([Coord], Int)], Field) -> ([([Coord], Int)], Field)
applyAllSteps2 (a@(([(-1,-1)], _):[]), field) = (a, field)
applyAllSteps2 (list, field) = applyAllSteps2 $ (foldr (\((lastBeam:_), num) acc -> combineUnique2 acc $ proceed2 lastBeam num field) [] list , field)

solution :: IO ()
solution = do
  fileContent <- readFile "input.txt"
  putStrLn $ show $ calculateNumberOfSplits $ snd $ applyAllSteps $ start $ parseField fileContent
  putStrLn $ show $ snd $ head $combineUnique2 [] $ fst $ applyAllSteps2 $ start2 $ parseField fileContent