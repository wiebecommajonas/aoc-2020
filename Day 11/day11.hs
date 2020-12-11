import System.Environment   
import Data.List

fst :: (a,b,c) -> a
fst x = case x of {(a,_,_) -> a}

snd :: (a,b,c) -> b
snd x = case x of {(_,a,_) -> a}

trd :: (a,b,c) -> c
trd x = case x of {(_,_,a) -> a}

parse :: [String] -> [[(Int, Int, Char)]]
parse seats = [[ (x,y,seat) | (x, seat) <- zip [0,1..] row] | (y, row) <- zip [0,1..] seats]

getAdjacent :: [[(Int, Int, Char)]] -> [[Int]]
getAdjacent seats = map (map (getAdjacent' seats)) seats

getAdjacent2 :: [[(Int, Int, Char)]] -> [[Int]]
getAdjacent2 seats = map (map (getAdjacent'' seats)) seats
        
getAdjacent' :: [[(Int, Int, Char)]] -> (Int, Int, Char) -> Int
getAdjacent' seats (x,y,c) = length [ 1 | xc <- [x-1 .. x+1], yc <- [y-1 .. y+1], not (xc==x && yc==y), not (xc < 0 || xc >= length (head seats)), not (yc < 0 || yc >= length seats), trd ((seats !! yc) !! xc) == '#']

getAdjacent'' :: [[(Int, Int, Char)]] -> (Int, Int, Char) -> Int
getAdjacent'' seats (x,y,c) = length $ filter (=='#') [ if trd ((seats !! yc) !! xc) == '.' then getAdjacentInDirection seats (x,y,c) (getDirection (x,y) (xc,yc)) else trd ((seats !! yc) !! xc) | xc <- [x-1 .. x+1], yc <- [y-1 .. y+1], not (xc==x && yc==y), not (xc < 0 || xc >= length (head seats)), not (yc < 0 || yc >= length seats)]

getDirection :: (Int,Int) -> (Int,Int) -> (Int,Int)
getDirection (currentx,currenty) (nextx,nexty) = (nextx-currentx,nexty-currenty)

getAdjacentInDirection :: [[(Int, Int, Char)]] -> (Int, Int, Char) -> (Int,Int) -> Char
getAdjacentInDirection seats (currentx,currenty,c) (dirx,diry) 
                                    | nextx < 0 || nextx >= length (head seats) || nexty < 0 || nexty >= length seats = c
                                    | trd next == '#' || trd next == 'L' = trd next
                                    | otherwise = getAdjacentInDirection seats next (dirx,diry)
                                    where
                                        nexty = currenty+diry
                                        nextx = currentx+dirx
                                        next = (seats !! nexty) !! nextx 

adjacent :: (Int, Int, Char) -> (Int, Int, Char) -> Bool
adjacent x seat = (fst x - 1 == fst seat || fst x + 1 == fst seat || fst x == fst seat) && (snd x - 1 == snd seat || snd x + 1 == snd seat || snd x == snd seat) && (not (fst x == fst seat && snd x == snd seat))
    where
        fst x = case x of {(a,_,_) -> a}
        snd x = case x of {(_,a,_) -> a}

changeState :: [[(Int, Int, Char)]] -> [[(Int, Int, Char)]]
changeState seats = [[ change' seat | seat <- row ] | row <- seats]
    where
        adjacent = getAdjacent seats
        change' :: (Int,Int,Char) -> (Int,Int,Char)
        change' (x,y,c)
            | (adjacent !! y) !! x == 0 && c == 'L' = (x,y,'#')
            | (adjacent !! y) !! x >= 4 && c == '#' = (x,y,'L')
            | otherwise = (x,y,c)

changeState2 :: [[(Int, Int, Char)]] -> [[(Int, Int, Char)]]
changeState2 seats = [[ change' seat | seat <- row ] | row <- seats]
    where
        adjacent = getAdjacent2 seats
        change' :: (Int,Int,Char) -> (Int,Int,Char)
        change' (x,y,c)
            | (adjacent !! y) !! x == 0 && c == 'L' = (x,y,'#')
            | (adjacent !! y) !! x >= 5 && c == '#' = (x,y,'L')
            | otherwise = (x,y,c)

runSeats :: [[(Int,Int,Char)]] -> [[(Int,Int,Char)]]
runSeats seats = h seats (changeState seats)
    where
        h seats' seats
            | seats == seats' = seats
            | otherwise = h seats (changeState seats)

runSeats2 :: [[(Int,Int,Char)]] -> [[(Int,Int,Char)]]
runSeats2 seats = h seats (changeState2 seats)
    where
        h seats' seats
            | seats == seats' = seats
            | otherwise = h seats (changeState2 seats)

countOccupied :: [[(Int,Int,Char)]] -> Int
countOccupied seats = (length . concat) [[ 1 | (_,_,c) <- row , c == '#'] | row <- seats]



main :: IO()
main = do
    args <- getArgs
    stringData <- readFile $ head args
    putStrLn $ "Solution Day 11 Part 1: " ++ show ( (countOccupied . runSeats . parse . lines) stringData ) -- 2238
    putStrLn $ "Solution Day 11 Part 2: " ++ show ( (countOccupied . runSeats2 . parse . lines) stringData )  -- 2013

