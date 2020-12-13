import System.Environment   
import Data.List

move :: [String] -> (Int,Int)
move instructions = h (0,0) 'E' instructions
    where
        h position _ [] = position
        h (x,y) dir (i:instructions) = case head i of
                                        'N' -> h (x,y+amount) dir instructions
                                        'E' -> h (x+amount,y) dir instructions
                                        'S' -> h (x,y-amount) dir instructions
                                        'W' -> h (x-amount,y) dir instructions
                                        'F' -> h (x,y) dir ((dir:tail i):instructions)
                                        'R' -> h (x,y) (turnRight amount dir) instructions
                                        'L' -> h (x,y) (turnRight (360 - amount) dir) instructions
                                        _ -> error "no instruction"
            where
                amount = (read :: String -> Int) $ tail i

move2 :: [String] -> (Int,Int)
move2 instructions = h (10,1) (0,0) 'E' instructions
    where
        h _ position _ [] = position
        h (wx,wy) (x,y) dir (i:instructions) = case head i of
                                        'N' -> h (wx,wy+amount) (x,y) dir instructions
                                        'E' -> h (wx+amount,wy) (x,y) dir instructions
                                        'S' -> h (wx,wy-amount) (x,y) dir instructions
                                        'W' -> h (wx-amount,wy) (x,y) dir instructions
                                        'F' -> h (wx,wy) (amount*wx+x,amount*wy+y) dir (instructions)
                                        'R' -> h (turnRight2 (wx,wy) amount) (x,y) dir instructions
                                        'L' -> h (turnRight2 (wx,wy) (360 - amount)) (x,y) dir instructions
                                        _ -> error "no instruction"
            where
                amount = (read :: String -> Int) $ tail i

turnRight2 :: (Int,Int) -> Int -> (Int,Int)
turnRight2 (a,b) am = ( round (cos rad) * a + round (sin rad) * b, round (-sin rad) * a + round (cos rad) * b)
    where
        amount = fromIntegral am
        rad = amount * pi / 180.0

turnRight :: Int -> Char -> Char
turnRight amount dir = case dir of 
                        'N' -> case amount of
                                    0 -> 'N'
                                    90 -> 'E'
                                    180 -> 'S'
                                    270 -> 'W'
                                    360 -> 'N'
                        'E' -> case amount of
                                    0 -> 'E'
                                    90 -> 'S'
                                    180 -> 'W'
                                    270 -> 'N'
                                    360 -> 'E'
                        'S' -> case amount of
                                    0 -> 'S'
                                    90 -> 'W'
                                    180 -> 'N'
                                    270 -> 'E'
                                    360 -> 'S'
                        'W' -> case amount of
                                    0 -> 'W'
                                    90 -> 'N'
                                    180 -> 'E'
                                    270 -> 'S'
                                    360 -> 'W'

manhattan :: (Int,Int) -> Int
manhattan (x,y) = abs x + abs y

main :: IO()
main = do
    args <- getArgs
    stringData <- readFile $ head args
    putStrLn $ "Solution Day 12 Part 1: " ++ show ( (manhattan . move . lines) stringData ) -- 362
    putStrLn $ "Solution Day 12 Part 2: " ++ show ( (manhattan . move2 . lines) stringData )  -- 29895

