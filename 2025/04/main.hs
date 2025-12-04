{-# LANGUAGE ScopedTypeVariables #-}

import Data.List.Split
import Data.Maybe
import Data.Map as Map
import Data.Typeable

main :: IO()
main = long 2

run :: String -> IO()
run file = do
    putStrLn "Part 1"
    grid <- readGrid . lines <$> readFile file
    --printGrid grid
    let poss = Prelude.filter ((=='@') . snd) $ checkWhere (fewer4Adj grid) grid
    let g' = Prelude.foldr (adjust (\x -> 'x') . fst) grid poss
    --pg g'
    print $ length poss
    --print poss
    return ()

run2 :: String -> IO()
run2 file = do
    putStrLn "Part 2"
    ls <- readGrid . lines <$> readFile file
    --printGrid ls
    count <- countRemove ls
    print count
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

pretty:: forall a . (Show a, Typeable a) => a -> String
pretty c = case cast c of
        Just (ch :: Char) -> [ch]
        Nothing           -> show c


sampleGrid = readGrid ["abcd", "efgh"]
s2 = readGrid [[1, 2, 3, 4], [5, 6, 7, 8]]

applyDir :: Dir -> Pos -> Pos
applyDir N (x, y) = (x, y-1)
applyDir E (x, y) = (x + 1, y)
applyDir S (x, y) = (x, y+1)
applyDir W (x, y) = (x -1, y)
applyDir NW p= applyDir N . applyDir W $ p
applyDir NE p= applyDir N . applyDir E $ p
applyDir SW p= applyDir S . applyDir W $ p
applyDir SE p= applyDir S . applyDir E $ p


enumElems :: Int -> Int -> [a] -> [((Int, Int), a)]
enumElems _ _ [] = []
enumElems x y (elem:t) = ((x, y), elem) : enumElems (x+1) y t

printGrid :: forall a. (Show a, Typeable a)  => Grid a -> IO()
printGrid grid = do
    let rows = [[pretty (grid ! (x, y)) | x <- [(minX grid)..(maxX grid)]] | y <- [(minY grid)..(maxY grid)]] :: [[String]]
    Prelude.mapM_ (putStrLn . concat) $ ["\n"]:rows++[[""]]

pg :: forall a. (Show a, Typeable a)  => Grid a -> IO()
pg = printGrid

maxX :: Grid a -> Int
maxX = maximum . Prelude.map (fst . fst) . toList

minX :: Grid a -> Int
minX = minimum . Prelude.map (fst . fst) . toList

maxY :: Grid a -> Int
maxY = maximum . Prelude.map (snd . fst) . toList

minY :: Grid a -> Int
minY = minimum . Prelude.map (snd . fst) . toList

readGrid :: [[a]] -> Grid a
readGrid ls = readLines 0 ls
    where
        readLines :: Int -> [[a]] -> Grid a
        readLines _ [] = Map.empty
        readLines y [line] = fromList $ enumElems 0 y line
        readLines y (line:ls) = union (fromList lineElems) $ readLines (y+1) ls
            where
                lineElems = enumElems 0 y line


all8Neighbours :: Eq a => Grid a -> Pos -> [(Pos, a)]
all8Neighbours grid pos = [(p, fromJust a) | (p, a) <- Prelude.filter ((/= Nothing) . snd) ex]
    where
        ex = Prelude.map (\dir -> (applyDir dir pos, Map.lookup (applyDir dir pos) grid)) [N, E, W, S, NE, NW, SE, SW]

fewer4Adj :: Grid Char -> (Pos, a) -> Bool
fewer4Adj grid (p, a) = (<4) . length . Prelude.filter (=='@') . Prelude.map snd $ all8Neighbours grid p

checkWhere :: Ord a => ((Pos, a) -> Bool) -> Grid a -> [(Pos, a)]
checkWhere cond grid = [((x, y), grid ! (x, y)) | x <- [(minX grid)..(maxX grid)],  y <- [(minY grid)..(maxY grid)], cond ((x, y), grid ! (x,y))]

countRemove :: Grid Char -> IO(Int)
countRemove grid = do
    let poss = Prelude.filter (fewer4Adj grid) . Prelude.filter ((=='@') . snd) $ toList grid
    let g' = Prelude.foldr (adjust (const 'x') . fst) grid poss

    --print $ "remove: " ++ (show (length poss))
    --printGrid g'

    if (Prelude.null poss) 
        then return 0 
        else do
            sub <- countRemove g' 
            return (sub + length poss)
    