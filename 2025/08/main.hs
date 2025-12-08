import Data.List.Split
import Data.List (sortBy, sort)
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.List.Duplicate as D
import Data.Map (Map)


main :: IO()
main = long 1

run :: String -> IO()
run file = do
    putStrLn "Part 1"
    entries <- sort . map toPoint . lines <$> readFile file
    let toWork = take 1000 . sortBy sortPair $ [(p1, p2) | p1 <- entries, p2 <- entries,  p2 > p1]

    let result = foldl addNetwork Map.empty  toWork

    let sizes = reverse . sort . map fromIntegral . networkSizes $ result
    print sizes

    print $ take 3 sizes
    let sol = product $ take 3 sizes
    print sol

    return ()

run2 :: String -> IO()
run2 file = do
    putStrLn "Part 2"
    entries <- sort . map toPoint . lines <$> readFile file
    let toWork = sortBy sortPair $ [(p1, p2) | p1 <- entries, p2 <- entries,  p2 > p1]

    let total = length entries

    print total

    let ins = addUntilOne Map.empty total toWork

    --print ins
    print $ last ins
    let ((a,_,_), (x,_,_)) = last ins

    let result = a * x
    print result



    return ()

short :: Int -> IO()
short part
    | part == 1 = run "s.txt"
    | otherwise = run2 "s.txt"

long :: Int ->  IO()
long part
    | part == 1 = run "l.txt"
    | otherwise = run2 "l.txt"

readInt :: String -> Int
readInt = read

pairId :: Pair -> (Int, Int)
pairId ((a, _, _), (b, _, _)) = (a,b)

type Point = (Int, Int, Int)
type Pair = (Point, Point)
type GroupMap = Map Point Int

toPoint :: String -> Point
toPoint line = (s!!0, s!!1, s!!2)
    where
        s = map readInt $ splitOn "," line

distance :: Point -> Point -> Float
distance (a,b,c) (x, y,z) = sqrt ((fr a -  fr x)^2 + (fr b - fr y )^2 + (fr c - fr z)^2)
    where
        fr = fromIntegral

sortPair :: Pair -> Pair -> Ordering
sortPair (a,b) (x,y)
    | distance a b >= distance x y = GT
    | otherwise = LT

addNetwork :: GroupMap -> Pair -> GroupMap
addNetwork groups (p1, p2)
    -- none inside yet -> create new network with new unique id
    | not memP1 && not memP2 = Map.union groups $ Map.fromList [(p1, nextId), (p2, nextId)]

    -- only one member -> insert other point into network of the member point
    | memP1 && not memP2 = Map.insert p2 (groups Map.! p1) groups
    | not memP1 && memP2 = Map.insert p1 (groups Map.! p2) groups

    -- both inside -> unify networks by updating all ids of the higher one to have the smaller network ID
    | otherwise = Map.map (\a -> if a==maxId then minId else a ) groups
    where
        memP1 = Map.member p1 groups
        memP2 = Map.member p2 groups
        nextId = if Map.size groups == 0 then 1 else List.maximum (Map.elems groups) + 1
        minId = min (groups Map.! p1 ) (groups Map.! p2)
        maxId = max (groups Map.! p1 ) (groups Map.! p2)

networkSizes :: GroupMap -> [Int]
networkSizes = map length . D.group  . Map.elems


addUntilOne :: GroupMap -> Int -> [Pair] -> [Pair]
addUntilOne _ _ [] = []
addUntilOne groups total (h:tl)
    | Map.size groups == total && length (D.group $ Map.elems groups) == 1 = []
    | otherwise = h : addUntilOne groups' total tl
    where
        groups' = addNetwork groups h