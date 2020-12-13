import System.Environment   
import Data.List

trees :: Int -> (Int,Int) -> [String] -> Int
trees _ _ [] = 0
trees pos slope (x:xs) 
                | x !! (pos `mod` length x) == '#'  = 1 + next
                | otherwise                         = 0 + next
                where
                    next = trees (pos+fst slope) (slope) (drop (snd slope) (x:xs))

multSlopes :: [(Int, Int)] -> [String] -> Int
multSlopes [] _ = 1
multSlopes (s:ss) xs = trees 0 s xs * multSlopes ss xs

main :: IO()
main = do
    args <- getArgs
    stringData <- readFile $ head args
    putStrLn $ "Solution Day 3 Part 1: " ++ show ( (trees 0 (3,1) . lines) stringData )
    putStrLn $ "Solution Day 3 Part 2: " ++ show ( (multSlopes [(1,1), (3,1), (5,1), (7,1), (1,2)] . lines) stringData )