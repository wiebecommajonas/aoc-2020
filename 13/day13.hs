import System.Environment   
import Data.List
import Data.List.Split (splitOn)

fst' :: (a,b,c) -> a
fst' x = case x of {(a,_,_) -> a}

snd' :: (a,b,c) -> b
snd' x = case x of {(_,a,_) -> a}

trd' :: (a,b,c) -> c
trd' x = case x of {(_,_,a) -> a}

deleteAll :: String -> [String] -> [String]
deleteAll del xs
        | del `elem` xs = deleteAll del (delete del xs)
        | otherwise = xs

parse :: [String] -> (Int, [Int])
parse ls = (a,busses)
    where
        a = (read :: String -> Int) $ head ls
        busses = map (read :: String -> Int) . deleteAll "x" . splitOn "," . last $ ls

parse' :: [String] -> [(Int, Int)]
parse' ls = busses
    where
        busses = map (\x -> (fst x, (read :: String -> Int) (snd x))) . filter (\x -> snd x /= "x") . zip [0..] . splitOn "," . last $ ls

busWithTime :: (Int, [Int]) -> [(Int, Int)]
busWithTime (earliest, busses) = [ (timestamp,b) | b <- sort busses, timestamp <- [earliest..earliest+maxWait], timestamp `mod` b == 0]
    where
        maxWait = maximum busses

result1 :: (Int, [Int]) -> Int
result1 (earliest, busses) = (lowestTimestamp - earliest) * busWithLowestTimestamp 
    where
        busTimes = busWithTime (earliest,busses)
        lowestTimestamp = minimum . map (\x -> fst x) $ busTimes
        busWithLowestTimestamp = snd . head . filter (\x -> fst x == lowestTimestamp) $ busTimes

result2 :: [(Int,Int)] -> Int -- brute force method (will take several days)
result2 bs = head [ timestamp | timestamp <- [1..], length [ 1 | (off,id) <- bs, not (timestamp == 0), (timestamp+off) `mod` id == 0, not (timestamp-off == 0)] == length bs]
    where
        step = minimum . map (\x -> snd x) $ bs

result3 :: [(Int, Int)] -> Int
result3 bs = (sum (map (\x -> fst' x * snd' x * trd' x) (zip3 as ns' us))) `mod` bigN
    where
        as = map (\x -> if fst x == 0 then 0 else snd x - fst x) bs
        ns = map snd bs
        bigN = product ns
        ns' = map (bigN `div`) ns
        us = map (\x -> head [u | u <- [1..snd x], (fst x * u) `mod` (snd x) == 1] ) (zip ns' ns)

main :: IO()
main = do
    args <- getArgs
    stringData <- readFile $ head args
    putStrLn $ "Solution Day 13 Part 1: " ++ show ( (result1 . parse . lines) stringData ) -- 4808
    putStrLn $ "Solution Day 13 Part 2: " ++ show ( (result3 . parse' . lines) stringData )  -- 741745043105674