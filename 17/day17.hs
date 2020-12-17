import System.Environment   
import Data.List
import Data.Set (Set)
import qualified Data.Set as S
import Data.Vector (Vector)
import qualified Data.Vector as V

type Point = V.Vector Int
type Configuration = S.Set Point -- representing active coordinates

parse :: String -> ([Int],[Int],[Int],Configuration)
parse str = (xr,yr,zr,(S.fromList . map V.fromList . concat) [ map (\(x,c) -> if c=='.' then [0,x,y,0] else [1,x,y,0]) (xAndChar l) | (y,l) <- yAndLine ])
    where
        split = lines str
        yAndLine = zip [0..] split
        xAndChar l = zip [0..] l
        xr = [0..length.head $ split]
        yr = [0..length split]
        zr = [0]

checkIfNeighbors :: Point -> Point -> Bool
checkIfNeighbors p1 p2 = (V.length . V.filter (\a -> a<=1)) (V.accumulate (\a1 a2 -> abs (a1 - a2)) (V.tail p1) (V.zip (V.fromList [0..V.length (V.tail p2)]) (V.tail p2))) == V.length (V.tail p1)

countActiveNeighbors :: Configuration -> Point -> Int
countActiveNeighbors conf point = S.size $ S.filter (\v -> V.head v == 1 && checkIfNeighbors point v) (S.delete point conf)

extendRange :: ([Int],[Int],[Int],Configuration) -> ([Int],[Int],[Int],Configuration)
extendRange (xrange,yrange,zrange,conf) = (xtdx, xtdy, xtdz, S.union conf (S.fromList [ V.fromList [0,x,y,z] | x <- xtdx, y <- xtdy, z <- xtdz, V.fromList [0,x,y,z] `S.notMember` conf, V.fromList [1,x,y,z] `S.notMember` conf]))
    where
        xtdx = xtdrange xrange
        xtdy = xtdrange yrange
        xtdz = xtdrange zrange
        xtdrange r = [minimum r -1..maximum r +1]

toggle :: Point -> Point
toggle p = V.update p (V.fromList [(0,if V.head p == 0 then 1 else 0)])

cycle' :: ([Int],[Int],[Int],Configuration) -> ([Int],[Int],[Int],Configuration)
cycle' (xrange,yrange,zrange,conf) = (xxr,xyr,xzr,S.map (\v -> if (V.head v == 0 && countActiveNeighbors xconf v == 3) || (V.head v == 1 && (countActiveNeighbors xconf v `notElem` [2,3])) then toggle v else v) xconf)
    where
        (xxr,xyr,xzr,xconf) = extendRange (xrange,yrange,zrange,conf)

cycleTimes :: Int -> ([Int],[Int],[Int],Configuration) -> Configuration
cycleTimes 0 (_,_,_,conf) = conf
cycleTimes n (xr,yr,zr,conf) = cycleTimes (n-1) (cycle' (xr,yr,zr,conf))

countActive :: Configuration -> Int
countActive conf = length $ S.filter (\v -> V.head v == 1) conf

--PART 2

parse4 :: String -> ([Int],[Int],[Int],[Int],Configuration)
parse4 str = (xr,yr,zr,wr,(S.fromList . map V.fromList . concat) [ map (\(x,c) -> if c=='.' then [0,x,y,0,0] else [1,x,y,0,0]) (xAndChar l) | (y,l) <- yAndLine ])
    where
        split = lines str
        yAndLine = zip [0..] split
        xAndChar l = zip [0..] l
        xr = [0..length.head $ split]
        yr = [0..length split]
        zr = [0]
        wr = [0]

extendRange4 :: ([Int],[Int],[Int],[Int],Configuration) -> ([Int],[Int],[Int],[Int],Configuration)
extendRange4 (xrange,yrange,zrange,wrange,conf) = (xtdx, xtdy, xtdz, xtdw, S.union conf (S.fromList [ V.fromList [0,x,y,z,w] | x <- xtdx, y <- xtdy, z <- xtdz, w <- xtdw, V.fromList [0,x,y,z,w] `S.notMember` conf, V.fromList [1,x,y,z,w] `S.notMember` conf]))
    where
        xtdx = xtdrange xrange
        xtdy = xtdrange yrange
        xtdz = xtdrange zrange
        xtdw = xtdrange wrange
        xtdrange r = [minimum r -1..maximum r +1]

cycle'4 :: ([Int],[Int],[Int],[Int],Configuration) -> ([Int],[Int],[Int],[Int],Configuration)
cycle'4 (xrange,yrange,zrange,wrange,conf) = (xxr,xyr,xzr,xwr,S.map (\v -> if (V.head v == 0 && countActiveNeighbors xconf v == 3) || (V.head v == 1 && (countActiveNeighbors xconf v `notElem` [2,3])) then toggle v else v) xconf)
    where
        (xxr,xyr,xzr,xwr,xconf) = extendRange4 (xrange,yrange,zrange,wrange,conf)

cycleTimes4 :: Int -> ([Int],[Int],[Int],[Int],Configuration) -> Configuration
cycleTimes4 0 (_,_,_,_,conf) = conf
cycleTimes4 n (xr,yr,zr,wr,conf) = cycleTimes4 (n-1) (cycle'4 (xr,yr,zr,wr,conf))

main :: IO()
main = do
    args <- getArgs
    stringData <- readFile $ head args
    putStrLn $ "Solution Day 17 Part 1: " ++ show ( (countActive . cycleTimes 6 . parse) stringData ) -- 27898
    putStrLn $ "Solution Day 17 Part 2: " ++ show ( (countActive . cycleTimes4 6. parse4) stringData) -- 2766491048287
