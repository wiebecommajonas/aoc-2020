import System.Environment   
import Data.List
import Data.Map (fromList,empty,insert)
import Data.Bits
import Data.List.Split (splitOn)

type Program = [(String, [(Int,Int)])]

decToBin :: Int -> [Int]
decToBin = go [] where
   go acc 0 = acc
   go acc n = let (d, m) = n `divMod` 2 in go (m : acc) d

binToDec :: [Bool] -> Int
binToDec = foldr (\x y -> fromEnum x + 2*y) 0

fillTo36 :: [Int] -> [Int]
fillTo36 bs
    | length bs == 36 = bs
    | otherwise = fillTo36 (0:bs)

takeUntilNextMask :: [String] -> [String]
takeUntilNextMask [] = []
takeUntilNextMask ls = h 0 ls
    where
        h c ls
            | c >= length ls = ls
            | (head . words . last . take (c+1)) ls /= "mask" = h (c+1) ls
            | otherwise = take c ls

parse :: [String] -> Program
parse [] = []
parse (l:ls)
    | head split == "mask" = (last split, parse' (takeUntilNextMask ls)):parse ls
    | otherwise = parse ls
    where
        split = words l

parse' :: [String] -> [(Int, Int)]
parse' [] = []
parse' (l:ls) = (adress,value):parse' ls
    where
        split = words l
        adress = (read :: String -> Int) . init . last . splitOn "[" . head $ split
        value = (read :: String -> Int) . last $ split

runProgram :: Program -> [(Int,Int)]
runProgram [] = []
runProgram (i:insts) = runMemsWithMask (fst i) (snd i) ++ runProgram insts

runMemsWithMask :: String -> [(Int,Int)] -> [(Int,Int)]
runMemsWithMask _ [] = []
runMemsWithMask mask (m:mems) = h mask m : runMemsWithMask mask mems
    where
        h :: String -> (Int,Int) -> (Int,Int)
        h mask (adress,value) = (adress, applyMask mask value)

applyMask :: String -> Int -> Int
applyMask mask num = (binToDec . reverse . map (\x -> if x == 0 then False else True)) (h mask ((fillTo36 . decToBin) num))
    where
        h [] _ = []
        h (m:mask) (d:num) = case m of 
                                'X' -> d: h mask num
                                '1' -> 1: h mask num
                                '0' -> 0: h mask num
                                _ -> error "incorrect input"

addMems :: [(Int,Int)] -> Int
addMems mems = foldr (+) 0 (fromList mems)

runProgram2 :: Program -> [(Int,Int)]
runProgram2 [] = []
runProgram2 (i:insts) = runMemsWithMask2 (fst i) (snd i) ++ runProgram2 insts

runMemsWithMask2 :: String -> [(Int,Int)] -> [(Int,Int)]
runMemsWithMask2 _ [] = []
runMemsWithMask2 mask (m:mems) = h mask m ++ runMemsWithMask2 mask mems
    where
        h :: String -> (Int,Int) -> [(Int,Int)]
        h mask (unmaskedAdress,value) = [(adress, value) | adress <- (getAdresses . applyMask2 mask) unmaskedAdress]

applyMask2 :: String -> Int -> String
applyMask2 mask num = (h mask ((fillTo36 . decToBin) num))
    where
        h [] _ = []
        h (m:mask) (d:num) = case m of 
                                'X' -> 'X': h mask num
                                '1' -> '1': h mask num
                                '0' -> (head . show) d: h mask num
                                _ -> error "incorrect input"

getAdresses :: String -> [Int]
getAdresses adress
            | not ('X' `elem` adress) = [(binToDec . reverse . map (\x -> if x == '0' then False else True)) adress]
            | otherwise = (concat . map getAdresses) [replaceFirst 'X' '0' adress, replaceFirst 'X' '1' adress]

replaceFirst :: Char -> Char -> String -> String
replaceFirst f r (s:ss)
            | s == f = r:ss
            | otherwise = s:replaceFirst f r ss

main :: IO()
main = do
    args <- getArgs
    stringData <- readFile $ head args
    putStrLn $ "Solution Day 14 Part 1: " ++ show ( (addMems . runProgram . parse . lines) stringData ) -- 14862056079561
    putStrLn $ "Solution Day 14 Part 2: " ++ show ( (addMems . runProgram2 . parse . lines) stringData ) -- 3296185383161
