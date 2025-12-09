import Data.List.Split
import Data.List (sortBy)
import qualified Data.List as List
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Sequence.Internal.Sorting (popMinTQ)
import Data.Data (Typeable)
import Data.Typeable (cast)

main :: IO()
main = long 2

run :: String -> IO()
run file = do
    putStrLn "Part 1"
    points <- map readLine . lines <$> readFile file
    --print points

    let pairs = sortBy (sortPair manhatten) [(p1, p2) | p1 <- points, p2 <- points, p2 > p1]
    --print pairs

    print $ recSize (last pairs)

    return ()

run2 :: String -> IO()
run2 file = do
    putStrLn "Part 2"
    points <- map readLine . lines <$> readFile file
    let points' = points ++ [head points]

    --print points'
    let insides = insidePoints $ reverse points'

    let fullGrid =  fillGrid (foldl markPointToFlood (fromList points') insides) '.'

    let origins = findElems fullGrid '@'
    let flooded = List.foldl' flood fullGrid origins

    --pg flooded


    let pars = sortBy (flip (sortPair manhatten)) ([(p1, p2) | p1 <- points, p2 <- points, p2 > p1])
    --print pars
    let sol = filter (uncurry (checkRec flooded)) pars

    let result = take 1 sol
    print result
    print $ recSize (head result)

    -- Part 2 is not working as expected.
    -- Another Idea would be to store each line segment as a line and for every pair of 2 red tiles, check if
    -- The 4 sides of a rectangle do not cut any line, or only lines that are directly followed by another line
    -- This solution should not require flooding the graph

    return ()

short :: Int -> IO()
short part
    | part == 1 = run "s.txt"
    | otherwise = run2 "s.txt"

long :: Int ->  IO()
long part
    | part == 1 = run "l.txt"
    | otherwise = run2 "l.txt"

type Point = (Int, Int)
type Pair = (Point, Point)
type Grid a = Map Point a

readLine :: String -> Point
readLine line = (read (n !! 0), read (n !! 1))
    where
        n =  splitOn "," line

distance :: Point -> Point -> Float
distance (a,b) (x, y) = sqrt ((fr a -  fr x)^2 + (fr b - fr y )^2)
    where
        fr = fromIntegral

manhatten :: Point -> Point -> Float
manhatten (a, b) (x, y) =  abs ( fr a - fr x) + 1 + abs (fr  b -  fr y) + 1
    where
        fr = fromIntegral


sortPair :: (Point -> Point -> Float) -> Pair -> Pair -> Ordering
sortPair distFn (a,b) (x,y)
    | distFn a b >= distFn x y = GT
    | otherwise = LT

recSize :: Pair -> Int
recSize ((a, b), (x, y)) = (abs (a - x) + 1) * (abs (b - y) + 1)

connect :: Grid Char -> Point -> Point -> Grid Char
connect g (a,b) (x,y)
    | a == x = Map.insert (a,b) '#' . Map.insert (x,y) '#' $ connectCol g (a,b) (y - b)
    | b == y = Map.insert (a,b) '#' . Map.insert (x,y) '#' $ connectRow g (a,b) (x - a)
    | otherwise = error "not on the same line"
    where
        connectCol :: Grid Char -> Point -> Int -> Grid Char
        connectCol grid _ 0 = grid
        connectCol grid (sx, sy) length
            | length > 0 = Map.insert (sx,sy) 'X' $ connectCol grid (sx, sy+1) (length-1)
            | length < 0 = Map.insert (sx,sy) 'X' $ connectCol grid (sx, sy-1) (length+1)

        connectRow :: Grid Char -> Point -> Int -> Grid Char
        connectRow grid _ 0 = grid
        connectRow grid (sx, sy) length
            | length > 0 = Map.insert (sx,sy) 'X' $ connectRow grid (sx+1, sy) (length-1)
            | length < 0 = Map.insert (sx,sy) 'X' $ connectRow grid (sx-1, sy) (length+1)


-- Given three points b a c, return the point that is diaganolly on the inside of a
-- b                b
-- |x              x|
-- a----c  |   c----a
insidePoint :: Point -> Point -> Point -> Point
insidePoint  (x,y) (a,b) (s,t)
    -- | x == a && y < b && s < a = (a-1, b-1)
    | x == a && y < b && s > a = (a+1, b-1)
    | x == a && y > b && s < a = (a-1, b+1)
    -- | x == a && y > b && s > a = (a+1, b+1)

    | y == b && x < a && t < b = (a - 1, b-1)
    -- | y == b && x < a && t > b = (a - 1, b+1)
    -- | y == b && x > a && t < b = (a + 1, b-1)
    | y == b && x > a && t > b = (a + 1, b+1)
    | otherwise = (a,b)
    -- | otherwise = error "invalid points"

flood :: Grid Char -> Point -> Grid Char
flood g start
    | not (Map.member start g) = g
    | not $ isBreak (g Map.! start) = List.foldl' flood (Map.insert start 'X' g) [(x-1, y), (x+1, y), (x, y-1), (x, y+1)]
    | otherwise = g
    where
        isBreak :: Char -> Bool
        isBreak c = c == '#' || c == 'X'
        (x,y) = start

checkRec :: Grid Char -> Point -> Point -> Bool
checkRec g (a,b) (x,y) = all (isMatch g) ([(lx, ly) | ly <- [min b y.. max b y], lx <- [a,x]] ++ [(lx, ly) | lx <- [min x a.. max a x], ly <- [b, y]])
    where
        isMatch :: Grid Char -> Point -> Bool
        isMatch g p = (g Map.! p) == 'X' || (g Map.! p) == '#'

insidePoints :: [Point] -> [Point]
insidePoints pts
    | length pts < 3 = []
    | otherwise = insidePoint x y z : insidePoints (y:z:tl)
    where
        x:y:z:tl = pts

markPointToFlood :: Grid Char -> Point -> Grid Char
markPointToFlood grid point
    | x == '#' || x == 'X' = grid
    | otherwise = Map.insert point '@' grid
    where
        x = if Map.member point grid then grid Map.! point else '.'


fromList :: [Point] -> Grid Char
fromList [] = Map.empty
fromList [a] = Map.empty
fromList (p1:p2:tl) = connect (fromList (p2:tl)) p1 p2

fillGrid :: Grid Char -> Char -> Grid Char
fillGrid g c = Map.union g $ Map.fromList [((x, y), c) | x <- [minX g .. maxX g], y <- [minY g .. maxY g]]


pretty:: forall a . (Show a, Typeable a) => a -> String
pretty c = case cast c of
        Just (ch :: Char) -> [ch]
        Nothing           -> show c


printGrid :: forall a. (Show a, Typeable a)  => Grid a -> IO()
printGrid grid = do
    let rows = [[pretty (grid Map.! (x, y)) | x <- [(minX grid)..(maxX grid)]] | y <- [(minY grid)..(maxY grid)]] :: [[String]]
    Prelude.mapM_ (putStrLn . concat) $ ["\n"]:rows++[[""]]

pg :: forall a. (Show a, Typeable a)  => Grid a -> IO()
pg = printGrid


maxX :: Grid a -> Int
maxX = maximum . Prelude.map (fst . fst) . Map.toList

minX :: Grid a -> Int
minX = minimum . Prelude.map (fst . fst) . Map.toList

maxY :: Grid a -> Int
maxY = maximum . Prelude.map (snd . fst) . Map.toList

minY :: Grid a -> Int
minY = minimum . Prelude.map (snd . fst) . Map.toList


findElems :: Eq a => Grid a -> a -> [Point]
findElems g a = map fst . Map.toList . Map.filter (==a) $ g