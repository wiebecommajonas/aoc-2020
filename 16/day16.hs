import System.Environment   
import Data.List
import Data.List.Split (splitOn)
import Data.Map (Map,fromList,map,filter,keys,size,empty,lookup,elems,union)

fst' :: (a,b,c) -> a
fst' x = case x of {(a,_,_) -> a}

snd' :: (a,b,c) -> b
snd' x = case x of {(_,a,_) -> a}

trd' :: (a,b,c) -> c
trd' x = case x of {(_,_,a) -> a}

type Ticket = [Int]
type Rules = Map String (Int->Bool)

parse :: String -> (Rules, Ticket, [Ticket])
parse string = (rules, myTicket, nearbyTickets)
    where
        splitNewL = splitOn "\n\n" string
        rulesLines = lines (head splitNewL)
        myTicket = Data.List.map (read::String->Int).splitOn ",".last.lines $ splitNewL !! 1
        nearbyTickets = Data.List.map (Data.List.map (read::String->Int).splitOn ",").tail.lines $ splitNewL !! 2
        rules = Data.Map.fromList [(head (split l), rule l) | l <- rulesLines]
            where 
                split line = splitOn ": " line
                rule line = (\x -> (\y -> (y>=(head.head) x && y<=(last.head) x) || (y>=(head.last) x && y<=(last.last) x))) $ Data.List.map (Data.List.map (read::String->Int).splitOn "-") (splitOn " or " (last (split line)))

atLeastOneRule :: Rules -> Int -> Bool
atLeastOneRule rules value = foldr (||) False $ Data.Map.map (\x -> x value) rules

matchingRules :: Rules -> Int -> [String]
matchingRules rules value = keys $ Data.Map.filter (\x -> x value) rules

validateTicket :: Rules -> Ticket -> Bool
validateTicket rules ticket = foldr (&&) True $ Data.List.map (atLeastOneRule rules) ticket

-- get every possible field for a specified index
possibleFields :: Int -> Rules -> [Ticket] -> [String]
possibleFields index rules tickets = foldr intersect (keys rules) $ Data.List.map (matchingRules rules) (Data.List.map (!!index) validTickets)
    where
        validTickets = Data.List.filter (validateTicket rules) tickets

-- possibleFields for every index
validFieldsAll :: Rules -> [Ticket] -> [[String]]
validFieldsAll rules tickets = [ possibleFields i rules tickets | i <- [0..ticketLength-1] ]
    where
        ticketLength = length (head tickets)

removeAll :: [String] -> [String] -> [String]
removeAll _ [] = []
removeAll x (y:ys)
    | elem y x = removeAll x ys
    | otherwise = y:removeAll x ys

fields :: Rules -> [Ticket] -> Map Int String
fields rules tickets = h empty vfa
    where
        vfa = validFieldsAll rules tickets
        element fields pf = fields \\ (foldr Data.List.union [] (pf \\ [fields]))
        h :: Map Int String -> [[String]] -> Map Int String
        h hmap pf 
            | size hmap >= length pf = hmap
            | otherwise = h newMap [if i `elem` keys newMap then [] else f | (i,f) <- zip [0..] removedPf]
            where
                newMap = Data.Map.union hmap (fromList [ (i, head (element fields pf)) | (i,fields) <- zip [0..] pf, (not.null) (element fields pf), not (i `elem` keys hmap)])
                removedPf = (Data.List.map (removeAll (elems newMap)) pf)

tser :: Rules -> [Ticket] -> Int
tser rules nearbyTickets = sum.concat $ [[ value | value <- ticket, not (atLeastOneRule rules value)] | ticket <- nearbyTickets]

multipliedDeps :: Map Int String -> Ticket -> Int
multipliedDeps fields ticket = product [ ticket !! k | k <- keysWithDep]
    where
        keysWithDep = keys (Data.Map.filter (\v -> (head.words) v == "departure") fields)

main :: IO()
main = do
    args <- getArgs
    stringData <- readFile $ head args
    putStrLn $ "Solution Day 16 Part 1: " ++ show ( ((\x -> tser (fst' x) (trd' x)) . parse) stringData ) -- 27898
    putStrLn $ "Solution Day 16 Part 2: " ++ show ( ((\x -> multipliedDeps (fields (fst' x) (trd' x)) (snd' x)) . parse) stringData ) -- 2766491048287
