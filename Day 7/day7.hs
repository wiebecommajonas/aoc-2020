import System.Environment   
import Data.List
import Data.Set (toList,fromList)

removeDuplicates :: Ord a => [a] -> [a]
removeDuplicates = toList . fromList

takeWhile' :: (a -> Bool) -> [a] -> [a]
takeWhile' _ [] = []
takeWhile' p (x:xs)
            | p x           = x : takeWhile' p xs
            | otherwise     = [x]

splitUp :: [String] -> [(String,[(Int,String)])]
splitUp [] = []
splitUp (l:ls) = (firstLevelBag, insideBags) : splitUp ls
    where
        split = words l
        firstLevelBag = unwords . take 2 $ split
        insideBagsAmount 
                    | length (drop 4 split) == 3    = 0
                    | otherwise                     = length (drop 4 split) `div` 4
        insideBags = h insideBagsAmount (drop 4 split)
            where
                h 0 _ = []
                h _ [] = []
                h c xs = (((read :: String -> Int) . head) xs, (unwords . tail . take 3) xs) : h (c-1) (drop 4 xs)

direct :: [(String,[(Int,String)])] -> [String]
direct m = map fst . filter (\a -> length (filter (\b -> snd b == "shiny gold") (snd a)) > 0 ) $ m

indirectly :: String -> [(String,[(Int,String)])] -> Bool
indirectly color m 
            | null $ bag                                            = False
            | length (filter (\a -> snd a == "shiny gold") bag) > 0 = True
            | otherwise                                             = indirectly' [c | (_,c) <- bag] m
            where
                bag = case lookup color m of
                                    Just xs -> xs
                                    Nothing -> []
                indirectly' :: [String] -> [(String,[(Int,String)])] -> Bool
                indirectly' [] _ = False
                indirectly' (c:cs) m = indirectly c m || indirectly' cs m

indirect :: [(String,[(Int,String)])] -> [String]
indirect m = removeDuplicates [x | (x,xx) <- m, (_,y) <- xx, not (x `elem` (direct m)), y `elem` (direct m) || indirectly y m]

emptyColors :: [(String,[(Int,String)])] -> [String]
emptyColors m = map fst . filter (\t -> null (snd t)) $ m

atLeastOne :: [(String,[(Int,String)])] -> [String]
atLeastOne m = direct m ++ indirect m

bagsInsideSG :: [(String,[(Int,String)])] -> Int
bagsInsideSG m = bagsInside' (bag "shiny gold")
    where
        bag c = case lookup c m of
                        Just xs -> xs
                        Nothing -> []
        bagsInside' :: [(Int, String)] -> Int
        bagsInside' [] = 0
        bagsInside' (b:bs) = fst b + fst b * bagsInside' (bag (snd b)) + bagsInside' bs

main :: IO()
main = do
    args <- getArgs
    stringData <- readFile $ head args
    putStrLn $ "Solution Day 3 Part 1: " ++ show ( ( length . atLeastOne . splitUp . lines) stringData)
    putStrLn $ "Solution Day 3 Part 2: " ++ show ( bagsInsideSG . splitUp . lines $ stringData )

