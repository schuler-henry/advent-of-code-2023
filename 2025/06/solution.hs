import Data.Char
import System.IO

parseNumberLine :: String -> [[Int]]
parseNumberLine line = map (\x -> [(read::String->Int) x]) (words line)

combineLists :: [[[Int]]] -> [[Int]]
combineLists xs = foldr (\lst acc -> zipWith (++) lst acc) (last xs) (init xs)

parseActionLine :: String -> [Char]
parseActionLine line = filter (not . isSpace) line

parseFile :: String -> [([Int], Char)]
parseFile content = (\lines -> zip (combineLists $ map parseNumberLine (init lines)) (parseActionLine (last lines))) (lines content)

applyAction :: [([Int], Char)] -> [Int]
applyAction list = foldr (\(nums, action) acc -> case action of
  '*' -> foldr (\n a -> n * a) 1 nums : acc
  '+' -> foldr (\n a -> n + a) 0 nums : acc
  _   -> acc) [] list

parseNumberLine2 :: String -> [[Char]]
parseNumberLine2 line = map (\x -> [x]) line

combineLines :: [[[Char]]] -> [[Char]]
combineLines xs = foldr (\line acc -> zipWith (++) line acc) (last xs) (init xs)

trimCombinedLines :: [[Char]] -> [[Char]]
trimCombinedLines = map (filter (not . isSpace))

splitOnEmpty :: [[Char]] -> [[Int]]
splitOnEmpty = foldr (\x acc -> if x == "" then [] : acc else ((read::String->Int) x : head acc) : tail acc) [[]]

parseFile2 :: String -> [([Int], Char)]
parseFile2 content = (\lines -> zip (splitOnEmpty $ trimCombinedLines $ combineLines $ map parseNumberLine2 $ init lines) (parseActionLine (last lines))) (lines content)


solution :: IO ()
solution = do
  fileContent <- readFile "input.txt"
  putStrLn $ show $ sum $ applyAction $ parseFile fileContent
  putStrLn $ show $ sum $ applyAction $ parseFile2 fileContent