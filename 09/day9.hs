import System.Environment   
import Data.List
import Data.Either

check :: Int -> [Int] -> Either Int Int
check n xs
    | index <= 25 = Right n
    | n `elem` [x+y | x <- list , y <- list , y /= x] = Right n
    | otherwise = Left n
    where
        index = head [i | (i,x) <- zip [1..] xs, x == n] 
        list = take 25 . drop (index-1-25) $ xs

checkAll :: [Int] -> Int
checkAll [] = -1
checkAll (x:xs) = h 0 (x:xs)
    where
        h c xs
            | c >= length xs = -1
            | isLeft (check (xs !! c) xs) = case check (xs !! c) xs of {Left n -> n; _ -> error "error"}
            | otherwise = h (c+1) xs

getFirstLeft :: [Either Int Int] -> Int
getFirstLeft [] = -1
getFirstLeft (x:xs) = case x of
                        Left n -> n
                        Right n -> getFirstLeft xs

getRange :: Int -> Int -> [Int] -> [Int]
getRange n1 n2 xs = [x | (i,x) <- zip [1..] xs, index1 >= i, index2 <= i]
    where
        index1 = head [i | (i,x) <- zip [1..] xs, x == n1] 
        index2 = head [i | (i,x) <- zip [1..] xs, x == n2] 

continuousSubSeqs :: [a] -> [[a]]
continuousSubSeqs = filter (not . null) . concatMap inits . tails

subsetThatSumsTo :: Int -> [Int] -> [Int]
subsetThatSumsTo n xs = h n (continuousSubSeqs xs)
    where
        h _ [] = []
        h n (x:xs)
            | sum x == n = x
            | otherwise = h n xs

sumMinAndMax :: [Int] -> Int
sumMinAndMax xs = minimum xs + maximum xs

main :: IO()
main = do
    args <- getArgs
    stringData <- readFile $ head args
    putStrLn $ "Solution Day 9 Part 1: " ++ show ( (checkAll . map (read :: String -> Int) . lines) stringData ) -- 1639024365
    let number = (checkAll . map (read :: String -> Int) . lines) stringData
    putStrLn $ "Solution Day 9 Part 2: " ++ show ( sumMinAndMax $ subsetThatSumsTo number ((map (read :: String -> Int) . lines) stringData) )  -- 219202240

