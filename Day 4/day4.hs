import System.Environment   
import Data.List
import Text.Read

-- splits list on last occurence of delimiter ans returns the first part
takeUntil :: Char -> String -> String
takeUntil delim str 
                | last str == delim = take (length str - 1) str
                | otherwise = takeUntil delim (take (length str - 1) str)

-- same as takeUntil, but returns other part
dropUntil :: Char -> String -> String
dropUntil delim str
                | head str == delim = drop 1 str
                | otherwise         = dropUntil delim (drop 1 str)

-- splits string on two consecutive occurences of given delimiter
splitOnTwice :: Char -> String -> [String]
splitOnTwice delim str = help 1 delim str
    where
        help :: Int -> Char -> String -> [String]
        help c delim str
                    | c == length str                               = [str]
                    | lastChar == delim && nextLastChar == delim    = take (c+1) str : help 1 delim (drop (c+1) str)
                    | otherwise                                     = help (c+1) delim str
            where
                lastChar = last $ take c str
                nextLastChar = last $ take (c+1) str

validPassport :: [String] -> [String] -> Bool
validPassport [] _ = True
validPassport (f:fields) xs = f `elem` xs && validPassport fields xs

validPassportAndData :: [String] -> [String] -> Bool
validPassportAndData [] _ = True
validPassportAndData (f:fields) xs = f `elem` passFields && validData f passData && validPassportAndData fields xs
    where
        passFields = map (takeUntil ':') xs
        passData = if (length . filter (\x -> takeUntil ':' x == f)) xs == 0 then "" else ((dropUntil ':') . head . filter (\x -> takeUntil ':' x == f)) xs

validData :: String -> String -> Bool
validData field fieldData = let d = (readMaybe fieldData :: Maybe Int) in case field of
                                "byr" -> length fieldData == 4 && case d of
                                                                    Just x -> x >= 1920 && x <= 2002
                                                                    Nothing -> False
                                "iyr" -> length fieldData == 4 && case d of
                                                                    Just x -> x >= 2010 && x <= 2020
                                                                    Nothing -> False
                                "eyr" -> length fieldData == 4 && case d of
                                                                    Just x -> x >= 2020 && x <= 2030
                                                                    Nothing -> False
                                "hgt" -> validHeight fieldData
                                "hcl" -> length fieldData == 7 && head fieldData == '#' && length (filter (\d -> d `elem` ['0'..'9']++['a'..'f']) (tail fieldData)) == length (tail fieldData)
                                "ecl" -> fieldData `elem` ["amb","blu","brn","gry","grn","hzl","oth"]
                                "pid" -> length fieldData == 9 && length (filter (\d -> d `elem` ['0'..'9']) fieldData) == 9
                                "cid" -> True
                                _     -> False

requiredFields :: [String]
requiredFields = ["byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid"]

validHeight :: String -> Bool
validHeight hgt 
            | end == "cm"   = case n of
                                Just x -> x >= 150 && x <= 193
                                Nothing -> False
            | end == "in"   = case n of
                                Just x -> x >= 59 && x <= 76
                                Nothing -> False
            | otherwise     = False 
            where
                n = readMaybe num :: Maybe Int
                end = drop (length hgt - 2) hgt
                num = take (length hgt - 2) hgt

main :: IO()
main = do
    args <- getArgs
    stringData <- readFile $ head args
    let passFields = (map words . splitOnTwice '\n') stringData
    putStrLn $ "Solution Day 3 Part 1: " ++ show ( (length . filter (validPassportAndData requiredFields)) passFields )
    putStrLn $ "Solution Day 3 Part 2: " ++ show ( (length . filter (validPassport requiredFields) . map (map (takeUntil ':'))) passFields )

