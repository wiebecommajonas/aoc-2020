import System.Environment   
import Data.List
import Data.Set (toList,fromList)

removeDuplicates :: Ord a => [a] -> [a]
removeDuplicates = toList . fromList

-- splits string on two consecutive occurences of given delimiter
splitOnTwice :: Char -> String -> [String]
splitOnTwice _ [] = []
splitOnTwice delim str = help 1 delim str
    where
        help :: Int -> Char -> String -> [String]
        help c delim str
                    | c == length str                               = [str]
                    | lastChar == delim && nextLastChar == delim    = element : help 1 delim (drop (c+1) str)
                    | otherwise                                     = help (c+1) delim str
            where
                lastChar = str !! (c-1)
                nextLastChar = str !! c
                element = take (c+1) str

rmEmpty :: [[a]] -> [[a]]
rmEmpty [] = []
rmEmpty (x:xs)
            | null x = rmEmpty xs
            | otherwise = x : rmEmpty xs

main :: IO()
main = do
    args <- getArgs
    stringData <- readFile $ head args
    putStrLn $ "Solution Day 6 Part 1: " ++ show ( (sum . map length . map removeDuplicates . map concat . map lines . splitOnTwice '\n') stringData )
    putStrLn $ "Solution Day 6 Part 2: " ++ show ( (sum . map length . map (foldr (intersect) ['a'..'z']) . map rmEmpty . map lines . splitOnTwice '\n') stringData )

