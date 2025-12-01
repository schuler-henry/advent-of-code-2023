import Data.Char
import System.IO

listToInt :: [Int] -> Int
listToInt = snd . foldr (\v (m, a) -> (m * 10, v * m + a)) (1, 0)

getLineValue :: String -> (Char, Int)
getLineValue (head:last) = (head, listToInt $ digitToInt <$> last)

applyLine :: (Char, Int) -> Int -> Int
applyLine (direction, value) acc = mod (if direction == 'R' then acc + value else acc - value) 100

getZeroPasses :: (Char, Int) -> Int -> Int
getZeroPasses (direction, value) acc = (\v -> if v <= 0 then fromIntegral $ toInteger $ ((truncate v * (-1)) + if acc == 0 then 0 else 1) else fromIntegral $ toInteger $ truncate v) (fromIntegral (if direction == 'R' then acc + value else acc - value) / 100)

addUp :: [(Char, Int)] -> (Int, Int)
addUp input = foldl (\(acc_val, occurrences) value -> (applyLine value acc_val, if applyLine value acc_val == 0 then occurrences + 1 else occurrences)) (50,0) input

addUpZeroPasses :: [(Char, Int)] -> (Int, Int)
addUpZeroPasses input = foldl(\(acc_val, occurrences) value -> (applyLine value acc_val, occurrences + getZeroPasses value acc_val)) (50, 0) input

solution :: IO ()
solution = do
  filecontent <- readFile "input.txt"
  putStrLn $ show $ addUp $ getLineValue <$> (lines filecontent)
  putStrLn $ show $ addUpZeroPasses $ getLineValue <$> (lines filecontent)