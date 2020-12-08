import System.Environment   
import Data.List 

-- Solution for day_1_input_jwiebe.txt: 691771

entriesThatAddTo2020 :: [Int] -> (Int,Int)
entriesThatAddTo2020 xs = head [(x,y) | x <- xs, y <- xs, x+y==2020]

multTuple :: (Int,Int) -> Int
multTuple (x,y) = x*y

main = do
    args <- getArgs
    contents <- readFile $ head args
    putStrLn $ "Solution Day 1 Part 1: " ++ show (multTuple . entriesThatAddTo2020 . map (read :: String -> Int) . words $ contents)