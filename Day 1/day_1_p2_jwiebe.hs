import System.Environment   
import Data.List 

-- Solution for day_1_input_jwiebe.txt: 232508760

threeEntriesThatAddTo2020 :: [Int] -> [Int]
threeEntriesThatAddTo2020 xs = head [[x,y,z] | x <- xs, y <- xs, z <- xs, x+y+z==2020]

main = do
    args <- getArgs
    contents <- readFile $ head args
    putStrLn $ "Solution Day 1 Part 2: " ++ show (product . threeEntriesThatAddTo2020 . map (read :: String -> Int) . words $ contents)