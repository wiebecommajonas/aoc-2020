import System.Environment   
import Data.List

splitAtFirstRL :: String -> (String, String)
splitAtFirstRL (s:ss) = help [] (s:ss)
    where
        help :: String -> String -> (String, String)
        help xs (s:ss)
                | s `elem` "RL" = (xs, (s:ss))
                | otherwise     = help (xs++[s]) ss

getSeatID :: (String, String) -> Int
getSeatID (r,c) = getRow rowsTotal r * 8 + getColumn columnsTotal c
    where
        getColumn :: [Int] -> String -> Int
        getColumn xs [] = head xs
        getColumn columns (c:cs)
                            | c == 'L'  = getColumn [head columns .. (length columns) `div` 2 - 1 + head columns] cs 
                            | c == 'R'  = getColumn [(length columns) `div` 2 + head columns .. last columns] cs 
                            | otherwise = getColumn columns cs
        getRow :: [Int] -> String -> Int
        getRow xs [] = head xs
        getRow rows (c:cs)
                        | c == 'F'  = getRow [head rows .. (length rows) `div` 2 - 1 + head rows] cs 
                        | c == 'B'  = getRow [(length rows) `div` 2 + head rows .. last rows] cs 
                        | otherwise = getRow rows cs

findSeatID :: [Int] -> Int
findSeatID ids = head [i+1 | i <- ids, j <- ids, i+2==j, not (i+1 `elem` ids)]


columnsTotal :: [Int]
columnsTotal = [0..7]

rowsTotal :: [Int]
rowsTotal = [0..127]

main :: IO()
main = do
    args <- getArgs
    stringData <- readFile $ head args
    putStrLn $ "Solution Day 5 Part 1: " ++ show ( (maximum . map getSeatID . map splitAtFirstRL . lines) stringData )
    putStrLn $ "Solution Day 5 Part 2: " ++ show ( (findSeatID . map getSeatID . map splitAtFirstRL . lines) stringData )

