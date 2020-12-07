import System.Environment   
import Data.List

takeUntil :: Char -> String -> String
takeUntil delim str 
                | last str == delim = take (length str - 1) str
                | otherwise = takeUntil delim (take (length str - 1) str)

dropUntil :: Char -> String -> String
dropUntil delim str
                | head str == delim = drop 1 str
                | otherwise         = dropUntil delim (drop 1 str)

xor :: Bool -> Bool -> Bool
xor x y | x == True && y == False = True
        | x == False && y == True = True
        | otherwise = False

validPw :: String -> Bool
validPw line = (amount >= min) && (amount <= max)
    where 
        split = words line
        min = (read :: String -> Int) $ takeUntil '-' (head split)
        max = (read :: String -> Int) $ dropUntil '-' (head split)
        pass = last split
        char = head $ split !! 1
        amount = length [c | c <- pass, c == char]

validPw2 :: String -> Bool
validPw2 line = xor ((pass !! min) == char) ((pass !! max) == char)
    where 
        split = words line
        min = ((read :: String -> Int) $ takeUntil '-' (head split)) - 1
        max = ((read :: String -> Int) $ dropUntil '-' (head split)) - 1
        pass = last split
        char = head $ split !! 1

main = do
    args <- getArgs
    contents <- readFile $ head args
    print $ "Aufgabe 1"
    print $ (length . filter (True==) . map validPw . lines) contents
    print $ "Aufgabe 2"
    print $ (length . filter (True==) . map validPw2 . lines) contents