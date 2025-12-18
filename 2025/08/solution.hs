import Data.Char
import System.IO

type Coord = (Int, Int, Int)
type Pair = (Coord, Coord)
type PairDistance = (Pair, Double)
type Circuit = [Coord]

parseFile :: String -> [Coord]
parseFile content = map parseLine (lines content)
  where parseLine line = let [x, y, z] = map read (splitAtChar ',' line) in (x, y, z)
        splitAtChar :: Char -> String -> [String]
        splitAtChar c str = foldr (\ch (acc:rest) -> if ch == c then "":acc:rest else (ch:acc):rest) [""] str

calcPairs :: [Coord] -> [Pair]
calcPairs coords = [(c1, c2) | (i, c1) <- zip [0..] coords, (j, c2) <- zip [0..] coords, i < j]

calcDistance :: Pair -> Double
calcDistance ((x1, y1, z1), (x2, y2, z2)) = sqrt (fromIntegral ((x2 - x1)^2 + (y2 - y1)^2 + (z2 - z1)^2))

appendDistance :: [Pair] -> [PairDistance]
appendDistance pairs = [(pair, calcDistance pair) | pair <- pairs]

-- merge sort implementation
mergeSort :: Ord b => (a -> b) -> [a] -> [a]
mergeSort f list = if length list <= 1 then list
                      else let (left, right) = splitAt (div (length list) 2) list
                           in merge (mergeSort f left) (mergeSort f right)
               where merge [] ys = ys
                     merge xs [] = xs
                     merge (x:xs) (y:ys) = if f x < f y
                                           then x : merge xs (y:ys)
                                           else y : merge (x:xs) ys


sortByDistance :: [PairDistance] -> [PairDistance]
sortByDistance list = mergeSort snd list

-- Check if c1 and c2 are already in any circuit
-- 0 are in any circuit => create new circuit
-- 1 is in a circuit => add the other to that circuit
-- 2 are in different circuits => merge the circuits
addPairToCircuits :: Pair -> [Circuit] -> [Circuit]
addPairToCircuits (c1, c2) circuits = case (findCircuit c1 circuits, findCircuit c2 circuits) of
  (Nothing, Nothing) -> [c1, c2] : circuits
  (Just idx1, Nothing) -> let (before, circuit:after) = splitAt idx1 circuits in before ++ ((c2:circuit):after)
  (Nothing, Just idx2) -> let (before, circuit:after) = splitAt idx2 circuits in before ++ ((c1:circuit):after)
  (Just idx1, Just idx2) | idx1 == idx2 -> circuits
                         | idx1 < idx2 -> let (before1, circuit1:rest) = splitAt idx1 circuits
                                              (before2, circuit2:after) = splitAt (idx2 - idx1 - 1) rest
                                          in before1 ++ before2 ++ ((circuit1 ++ circuit2) : after)
                         | idx2 < idx1 -> let (before2, circuit2:rest) = splitAt idx2 circuits
                                              (before1, circuit1:after) = splitAt (idx1 - idx2 - 1) rest
                                          in before2 ++ before1 ++ ((circuit1 ++ circuit2) : after)
  where findCircuit _ [] = Nothing
        findCircuit coord (circuit:rest) = if coord `elem` circuit then Just 0
                                           else case findCircuit coord rest of
                                                  Nothing -> Nothing
                                                  Just idx -> Just (idx + 1)


createCircuitForFirstNPairs :: Int -> [PairDistance] -> [Circuit] -> [Circuit]
createCircuitForFirstNPairs 0 _ circuits = circuits
createCircuitForFirstNPairs n ((pair,_):xs) circuits = createCircuitForFirstNPairs (n-1) xs (addPairToCircuits pair circuits)

getCircuitsForFirstNPairs :: Int -> [PairDistance] -> [Circuit]
getCircuitsForFirstNPairs n pairsWithDistances = createCircuitForFirstNPairs n pairsWithDistances []

getLargestNCircuitsForFirstMPairs :: Int -> Int -> [PairDistance] -> [Circuit]
getLargestNCircuitsForFirstMPairs n m pairsWithDistances = let sortedCircuits = mergeSort length $ getCircuitsForFirstNPairs m pairsWithDistances
                                                           in snd $ splitAt (length sortedCircuits - n) sortedCircuits


-- Part 2
createCircuitUntilAllInOneCircuit :: Int -> [PairDistance] -> ([Circuit], Pair) -> ([Circuit], Pair)
createCircuitUntilAllInOneCircuit _ [] circuits = circuits
createCircuitUntilAllInOneCircuit numBoxes ((pair,_):xs) (circuits, lastAppliedPair) = if length circuits == 1 && length (head circuits) == numBoxes
                                                                      then (circuits, lastAppliedPair)
                                                                      else createCircuitUntilAllInOneCircuit numBoxes xs ((addPairToCircuits pair circuits), pair)

getLastConnectedPairToFormSingleCircuit :: [Coord] -> Pair
getLastConnectedPairToFormSingleCircuit boxes = snd $ createCircuitUntilAllInOneCircuit (length boxes) (sortByDistance $ appendDistance $ calcPairs boxes) ([], ((0,0,0),(0,0,0)))

multiplyXCoordinates :: Pair -> Int
multiplyXCoordinates ((x1, _, _), (x2, _, _)) = x1 * x2

solution :: IO ()
solution = do
  fileContent <- readFile "input.txt"
  -- For validation we need to process 10 pairs, for input we need to process 1000 pairs
  putStrLn $ show $ product $ map length $ getLargestNCircuitsForFirstMPairs 3 1000 $ sortByDistance $ appendDistance $ calcPairs $ parseFile $ fileContent
  putStrLn $ show $ multiplyXCoordinates $ getLastConnectedPairToFormSingleCircuit $ parseFile $ fileContent 