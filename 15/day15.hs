import System.Environment   
import Data.List

getLastIndexOf :: Int -> [Int] -> Int
getLastIndexOf x xs
	| null list = length xs
	| otherwise = last list
	where
		list = init [a | (a,b) <- zip [1..] xs, b==x]

turn :: [Int] -> [Int]
turn xs = xs++[indexOfLastTurn - lastIndexOfNum]
	where
		lastNum = last xs
		indexOfLastTurn = length xs
		lastIndexOfNum = getLastIndexOf lastNum xs

turnWhile :: (Int -> Bool) -> [Int] -> [Int]
turnWhile p xs
	| p (length xs) = turnWhile p (turn xs)
	| otherwise = xs

main :: IO()
main = do
    args <- getArgs
    stringData <- readFile $ head args
    putStrLn $ "Solution Day 15 Part 1: " ++ show ( (last . turnWhile (<2020) . map (read :: String -> Int) . lines) stringData ) -- 14862056079561
    putStrLn $ "Solution Day 15 Part 2: " ++ show ( (last . turnWhile (<30000000) . map (read :: String -> Int) . lines) stringData ) -- 3296185383161
