import System.Environment   
import Data.List
import Data.Either

data Command = Acc Int | Jmp Int | Nop Int deriving (Show)

read' :: String -> Command
read' str = case head . words $ str of
                "acc" -> Acc number
                "jmp" -> Jmp number
                "nop" -> Nop number
                where
                    number = (read :: String -> Int) . (\x -> if head x == '+' then tail x else x) . last . words $ str

instance Read Command where
    readsPrec _ r = [(read' r, "")]

execute :: Int -> Int -> [Int] -> [Command] -> Either Int Int
execute acc counter counterHistory cmds
    | length cmds <= counter = Right acc
    | (length . group . sort) counterHistory < length counterHistory = Left acc
    | otherwise = case (cmds !! counter) of
                    Acc x -> execute (acc+x) (counter+1) (counter+1 : counterHistory) cmds
                    Jmp x -> execute acc (counter+x) (counter+x : counterHistory) cmds
                    Nop x -> execute acc (counter+1) (counter+1 : counterHistory) cmds

execute' :: Int -> Int -> [Int] -> [Command] -> Int
execute' acc counter counterHistory cmds
    | length cmds <= counter = acc
    | (length . group . sort) counterHistory < length counterHistory = error "Program does not end"
    | otherwise = case (cmds !! counter) of
                    Acc x -> execute' (acc+x) (counter+1) (counter+1 : counterHistory) cmds
                    Jmp x -> if isRight (executeJmp x) || isLeft (executeNop)
                                then execute' acc (counter+x) (counter+x : counterHistory) cmds
                                else execute' acc (counter+1) (counter+1 : counterHistory) cmds
                    Nop x -> if isRight executeNop || isLeft (executeJmp x)
                                then execute' acc (counter+1) (counter+1 : counterHistory) cmds
                                else execute' acc (counter+x) (counter+x : counterHistory) cmds
    where
        executeNop = execute acc (counter+1) (counter+1 : counterHistory) cmds
        executeJmp x = execute acc (counter+x) (counter+x : counterHistory) cmds

main :: IO()
main = do
    args <- getArgs
    stringData <- readFile $ head args
    putStrLn $ "Solution Day 8 Part 1: " ++ show ( ((\x -> case x of {Right x -> x; Left x -> x}) . execute 0 0 [0] . map (read :: String -> Command) . lines) stringData ) -- 1594
    putStrLn $ "Solution Day 8 Part 2: " ++ show ( (execute' 0 0 [0] . map (read :: String -> Command) . lines) stringData )      -- 758

