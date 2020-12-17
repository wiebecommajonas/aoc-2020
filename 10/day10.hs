import System.Environment   
import Data.List
import Data.Either
import Data.Tree
import Data.Set (toList,fromList)

removeDuplicates :: Ord a => [a] -> [a]
removeDuplicates = toList . fromList

getNextJoltageAndDifference :: Int -> [Int] -> Either String (Int, Int)
getNextJoltageAndDifference jolt js = if length possibleNexts == 0 then Left "Nothing found" else Right (minimum possibleNexts)
    where
        possibleNexts = [(j,abs (j - jolt)) | j <- js , abs (j - jolt) <= 3, abs (j - jolt) >= 1]

possibleArrangements :: [Int] -> Int
possibleArrangements [] = 0
possibleArrangements [j] = 1
possibleArrangements js = possibleArrangements (init js) 

leading :: Int -> [Int] -> [[Int]]
leading x js = map (filter (x==)) [[jj | jj <- js, j-jj >= 1, j-jj <= 3] | j <- js]

getAllNextJs :: Int -> [Int] -> [Int]
getAllNextJs _ [] = []
getAllNextJs jolt js = [j | j <- js , j - jolt <= 3, j - jolt >= 1]

countArrangements :: Tree Int -> Int
countArrangements (Node a []) = 1
countArrangements (Node _ forrest) = sum $ map (countArrangements) forrest

getDifferences :: Int -> [Int] -> [(Int, Int)]
getDifferences j js = h j [] js
    where
        h :: Int -> [(Int, Int)] -> [Int] -> [(Int, Int)]
        h _ diffs [] = diffs
        h currentJ diffs js
            | nextJoltage == -1 = diffs
            | otherwise = h nextJoltage ((nextJoltage, difference):diffs) (delete nextJoltage js)
            where
                (nextJoltage, difference) = case getNextJoltageAndDifference currentJ js of
                                                                Right x -> x
                                                                Left x -> (-1,-1)

result1 :: [(Int, Int)] -> Int
result1 jsds = (length . filter (\x -> snd x == 1)) jsds * ((length . filter (\x -> snd x == 3)) jsds + 1)

main :: IO()
main = do
    args <- getArgs
    stringData <- readFile $ head args
    putStrLn $ "Solution Day 10 Part 1: " ++ show ( (result1 . getDifferences 0 . map (read :: String -> Int) . lines) stringData ) -- 2070
--    let maxJ = maximum $ (map (read :: String -> Int) . lines) stringData
--    print $ maxJ
--    putStrLn $ "Solution Day 10 Part 2: " ++ show ( (leading 19 . sort . map (read :: String -> Int) . lines) stringData )  -- 219202240

