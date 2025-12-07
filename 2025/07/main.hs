import Data.List.Split
import Data.Data (Typeable, cast)
import qualified Data.Map as Map
import Data.Map (Map, (!))

main :: IO()
main = long 2

run :: String -> IO()
run file = do
    putStrLn "Part 1"
    g <- readGrid . lines <$> readFile file
    --printGrid g
    let splitters =  findElems g '^'

    let used = filter (validSplitter g) splitters

    -- +1 because the top most splitter is not recognized valid
    print $ length used + 1

    return ()

run2 :: String -> IO()
run2 file = do
    putStrLn "Part 2"
    
    g <- readGrid . lines <$> readFile file

    --pg g

    let start =  findElems g 'S'
    --print start

    let t = timelines g $ head start
    print t

    return ()

short :: Int -> IO()
short part
    | part == 1 = run "s.txt"
    | otherwise = run2 "s.txt"

long :: Int ->  IO()
long part
    | part == 1 = run "l.txt"
    | otherwise = run2 "l.txt"


data Dir = N | E | W | S |NE | NW | SE | SW deriving Show

type Pos = (Int, Int)
type Grid a = Map Pos a


findElems :: Eq a => Grid a -> a -> [Pos]
findElems g a = map fst . Map.toList . Map.filter (==a) $ g


validSplitter :: Grid Char -> Pos -> Bool
validSplitter g pos = (g ! pos) == '^' && inUse g (applyDir N pos)

inUse :: Grid Char -> Pos -> Bool
inUse g pos = validPos g pos && (g!pos) /= '^' &&(lrSplitter g pos || inUse g (applyDir N pos))
    where
        lpos = applyDir W pos
        rpos = applyDir E pos

        lrSplitter :: Grid Char -> Pos -> Bool
        lrSplitter g pos = validPos g lpos && ((g!lpos) == '^') || validPos g rpos && ((g!rpos) == '^')

isSplitter :: Grid Char -> Pos -> Bool
isSplitter g p = validPos g p && (g ! p) == '^'

times :: Grid Int -> Grid Char -> Pos -> (Grid Int, Int)
times ram grid pos 
    | Map.member pos ram = (ram, ram ! pos)
    | not (isSplitter grid pos) = if validPos grid pos then times ram grid (applyDir S pos) else (ram, 1)
    | otherwise = (Map.insert pos amount ram', amount)
        where
            amount = lamount + ramount
            (ramL, lamount) = times ram grid (applyDir W pos)
            (ram', ramount) = times ramL grid (applyDir E pos)

timelines :: Grid Char -> Pos -> Int
timelines grid pos = snd $ times Map.empty grid pos


pretty:: forall a . (Show a, Typeable a) => a -> String
pretty c = case cast c of
        Just (ch :: Char) -> [ch]
        Nothing           -> show c


applyDir :: Dir -> Pos -> Pos
applyDir N (x, y) = (x, y-1)
applyDir E (x, y) = (x + 1, y)
applyDir S (x, y) = (x, y+1)
applyDir W (x, y) = (x -1, y)
applyDir NW p= applyDir N . applyDir W $ p
applyDir NE p= applyDir N . applyDir E $ p
applyDir SW p= applyDir S . applyDir W $ p
applyDir SE p= applyDir S . applyDir E $ p

validPos :: Grid a -> Pos -> Bool
validPos = flip Map.member

hasNeighbour :: Grid a -> Dir -> Pos -> Bool
hasNeighbour g d p = x >= minX g && x <= maxX g && y >= minY g && y <= maxY g
    where
        (x, y) = applyDir d p

printGrid :: forall a. (Show a, Typeable a)  => Grid a -> IO()
printGrid grid = do
    let rows = [[pretty (grid ! (x, y)) | x <- [(minX grid)..(maxX grid)]] | y <- [(minY grid)..(maxY grid)]] :: [[String]]
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


enumElems :: Int -> Int -> [a] -> [((Int, Int), a)]
enumElems _ _ [] = []
enumElems x y (elem:t) = ((x, y), elem) : enumElems (x+1) y t

readGrid :: [[a]] -> Grid a
readGrid ls = readLines 0 ls
    where
        readLines :: Int -> [[a]] -> Grid a
        readLines _ [] = Map.empty
        readLines y [line] = Map.fromList $ enumElems 0 y line
        readLines y (line:ls) = Map.union (Map.fromList lineElems) $ readLines (y+1) ls
            where
                lineElems = enumElems 0 y line