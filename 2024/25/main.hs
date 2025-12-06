import Data.List.Split (splitOn)
import Data.List as List
import Prelude as Pr

data Pin = Key {
      c1 :: Int
    , c2 :: Int
    , c3 :: Int
    , c4 :: Int
    , c5 :: Int
    }
    | Lock {
      c1 :: Int
    , c2 :: Int
    , c3 :: Int
    , c4 :: Int
    , c5 :: Int
    }
    deriving (Show, Eq)

p1 = do
    contents <- readFile "l.txt"
    print contents
    let pins = Pr.map parseData . splitOn [""] $ lines contents
    let (keys, locks) = splitKeyLock pins
    --print locks 
    --print keys

    let matches = Pr.map (matchCount noOverlap locks) keys
    print matches
    print $ sum matches




matchCount :: (Pin -> Pin -> Bool) -> [Pin] -> Pin -> Int
matchCount fn locks key = Pr.foldl (\s lock -> if fn key lock then s+1 else s) 0 locks

noOverlap :: Pin -> Pin -> Bool
noOverlap (Key a b c d e) (Lock x y z w v) = all (<=5) [a+x, b+y, c+z, d+w, e+v]
noOverlap _ _ = False

match :: Pin -> Pin -> Bool
match (Key a b c d e) (Lock x y z w v) = all (==5) [a+x, b+y, c+z, d+w, e+v]
match _ _ = False

splitKeyLock :: [Pin] -> ([Pin], [Pin])
splitKeyLock [] = ([], [])
splitKeyLock (p:ps) = case p of
     (Key a b c d e) -> ((Key a b c d e):keys, locks)
     (Lock a b c d e) -> (keys, (Lock a b c d e):locks)
    where
        (keys, locks) = splitKeyLock ps

parseData :: [String] -> Pin
parseData ((c:cc):ss) 
    | c == '#' = parseLock ss
    | otherwise = parseKey ss
    where
        parse :: [String] -> (Int, Int, Int, Int, Int)
        parse ss = to5Tuple . Pr.map (count '#') . List.transpose $ ss
        parseKey ss = let (a, b, c, d, e) = parse ss in Key (a-1) (b-1) (c-1) (d-1) (e-1)
        parseLock ss = let (a, b, c, d, e) = parse ss in Lock a b c d e


count :: Eq a => a -> [a] -> Int
count _ [] = 0
count c (s:ss)
    | s == c = 1 + count c ss
    | otherwise = count c ss

to5Tuple :: [a] -> (a, a, a, a, a)
to5Tuple [a, b, c, d, e] = (a, b, c, d, e)