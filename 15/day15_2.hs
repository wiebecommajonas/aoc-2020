import System.Environment   
import Data.List
import Data.IntMap (IntMap,lookup,fromList,insert)

type Cache = IntMap Int

cacheFrom :: [Int] -> Cache
cacheFrom xs = Data.IntMap.fromList [(x,i) | (i,x) <- zip [1..] xs]

turn :: (Cache, Int, Int) -> (Cache, Int, Int)
turn (cache, lastIndex, lastNum) = (nextCache, nextIndex, nextNum)
	where
		nextIndex = lastIndex+1
		lastIndexOfNum = case Data.IntMap.lookup lastNum cache of
									Just x -> x
									Nothing -> lastIndex
		nextNum = lastIndex - lastIndexOfNum
		nextCache = Data.IntMap.insert lastNum lastIndex cache

turnWhile :: (Int -> Bool) -> (Cache, Int, Int) -> Int
turnWhile p (cache, lastIndex, lastNum)
	| p (lastIndex) = turnWhile p (turn (cache, lastIndex, lastNum))
	| otherwise = lastNum

main :: IO()
main = do
    args <- getArgs
    stringData <- readFile $ head args
    let limit = args !! 1
    putStrLn $ "Solution Day 15 Part 1: " ++ show ( ((\x -> turnWhile (<2020) (cacheFrom x, length x, last x)) . map (read :: String -> Int) . lines) stringData ) -- 1373
    putStrLn $ "Solution Day 15 Part 2: " ++ show ( ((\x -> turnWhile (<30000000) (cacheFrom x, length x, last x)) . map (read :: String -> Int) . lines) stringData ) -- 112458
