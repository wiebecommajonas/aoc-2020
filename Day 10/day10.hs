import System.Environment   
import Data.List
import Data.Either
import Data.Tree

getNextJoltageAndDifference :: Int -> [Int] -> Either String (Int, Int)
getNextJoltageAndDifference jolt js = if length possibleNexts == 0 then Left "Nothing found" else Right (minimum possibleNexts)
    where
        possibleNexts = [(j,abs (j - jolt)) | j <- js , abs (j - jolt) <= 3, abs (j - jolt) >= 1]

getAllNextJs :: Int -> [Int] -> [Int]
getAllNextJs _ [] = []
getAllNextJs jolt js = [j | j <- js , j - jolt <= 3, j - jolt >= 1]

getAllArrangements :: [Int] -> Int
getAllArrangements js = h 0 js (getAllNextJs 0 js)
    where
        h :: Int -> [Int] -> [Int] -> Int
        h _ _ [] = 1
        h j js njs = sum $ map (\x -> h x (delete x js) (getAllNextJs x (delete x js))) njs

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
    putStrLn $ "Solution Day 9 Part 1: " ++ show ( (result1 . getDifferences 0 . map (read :: String -> Int) . lines) stringData ) -- 2070
    putStrLn $ "Solution Day 9 Part 2: " ++ show ( (getAllArrangements . map (read :: String -> Int) . lines) stringData )  -- 219202240

