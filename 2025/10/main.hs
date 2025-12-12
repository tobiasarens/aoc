import Data.List.Split
import Data.Bits
import Data.List (foldl')
import qualified Data.List as L
import qualified Data.Maybe as Maybe
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map (Map)
import qualified Data.Map as Map

import Data.Heap (Heap)
import qualified Data.Heap as Heap

main :: IO()
main = long 1

run :: String -> IO()
run file = do
    putStrLn "Part 1"
    ls <-  lines <$> readFile file
    let machines = map parseMachine ls

    shortest <- solveMachine 1 machines

    --let shortest = map ((+ negate 1) . length . flip shortestPath 0) machines

    --print shortest
    print $ sum shortest

    return ()

solveMachine :: Int -> [Machine] -> IO([Int])
solveMachine _ [] = return []
solveMachine num (m:remain) = do
            let sol = length (shortestPath m 0) - 1
            putStrLn $ "Nr: " ++ show num ++ " - result: " ++show sol ++" - " ++ show m
            rest <- solveMachine (num+1) remain
            return (sol : rest)




run2 :: String -> IO()
run2 file = do
    putStrLn "Part 2"
    ls <-  lines <$> readFile file
    print ls
    return ()

short :: Int -> IO()
short part
    | part == 1 = run "s.txt"
    | otherwise = run2 "s.txt"

long :: Int ->  IO()
long part
    | part == 1 = run "l.txt"
    | otherwise = run2 "l.txt"

type Lights = Int
type Button = Lights -> Lights


-- Machine contains the amount of lights, the target lights and a list of buttons to press
type Machine = (Int, Lights, [Button])

instance Show Button where
    show b = showLights 6 $ b 0

showLights :: Int -> Lights -> String
showLights count lights = "[" ++ [cOfp p | p <- [0..(count-1)]] ++ "]"
    where
        cOfp p = if lights `testBit` p then '#' else '.'

applyPresses :: Machine -> Int -> [Int] -> String
applyPresses (c1, _, btns) l [] = showLights c1 l
applyPresses (c1, t, btns) l (b:tl) = applyPresses (c1, t, btns) ((btns !! b) l) tl

pushButton :: Button -> Lights -> Lights
pushButton b = b

toggleLightAt :: Lights -> Int -> Lights
toggleLightAt = complementBit

makeButton :: [Int] -> Button
makeButton toggles light = foldl' toggleLightAt light toggles

parseMachine :: String -> Machine
parseMachine line = (lCount, target, buttons)
    where
        lights = splitOn "]" line
        (lCount, target) = parseLights . drop 1 $ head lights
        buttonInp = splitOn "{" (lights !! 1)

        buttons = parseButtons $ head buttonInp

        parseLights :: String -> (Int, Lights)
        parseLights inp = (length inp, pLd inp)
            where
                pLd:: [Char] -> Lights
                pLd [] = 0
                pLd (h:tl) = pLd tl `shiftL` 1 + if h == '#' then 1 else 0

        parseButtons :: String -> [Button]
        parseButtons inp = map (makeButton .  parseWord . splitOn "," . map repl) $ words inp
            where
                repl '(' = ' '
                repl ')' = ' '
                repl x = x
                parseWord :: [String] -> [Int]
                parseWord [] = []
                parseWord (h:tl)
                    | h == "(" || h == ")" =  parseWord tl
                    | otherwise = read h : parseWord tl

data Distance a = Dist a | Infinity
    deriving (Show, Eq)

instance (Ord a ) => Ord (Distance a) where
    Infinity <= Infinity = True
    Infinity <= Dist x = False
    Dist x <= Infinity = True
    Dist x <= Dist y = x <= y

addDist :: (Num a) => Distance a -> Distance a -> Distance a
addDist (Dist x) (Dist y) = Dist (x+y)
addDist _ _ = Infinity

data BFSState = BFS {
    visited :: Set Lights,
    queue :: [Lights],
    -- queue :: Heap (Entry Int Lights),
    prec :: Map Lights Lights,
    found :: Bool
}

instance Show BFSState where
    show (BFS v q p f) = "visited: " ++ show v ++ "\nqueue: " ++ show q ++ "\nprec: " ++ show p ++ "\nFINISHED: " ++ show f ++ "\n"

startBFSState :: Lights -> BFSState
startBFSState start = BFS Set.empty [start] Map.empty False

bfsStep :: Machine -> BFSState -> BFSState
bfsStep machine (BFS v q p f)
    | f = BFS v q p f
    | L.null q = BFS v q p False
    | found = BFS (Set.insert current v) q precs True
    | otherwise = BFS (Set.insert current v) queue' precs f
    where
        (c, target, btns) = machine
        (current:queue) = q
        reach = reachable btns current
        found = target `elem` reach
        queue' = queue ++ reach
        precs = Map.union p $ Map.fromList [(r, current) | r <- reach]

bfs :: Machine -> BFSState -> BFSState
bfs machine (BFS v q p f)
    | f = BFS v q p f
    | L.null q = BFS v q p False
    | Set.member current v = bfs machine (BFS v queue p f)
    | found = BFS (Set.insert current v) q (Map.insert target current p) True
    | otherwise = bfs (c, target, btns) nextState
    where
        (c, target, btns) = machine
        (current:queue) = q
        reach = reachable btns current
        found = target `elem` reach
        nextState = BFS (Set.insert current v) (queue ++ reach) (foldl' (\acc l -> Map.insert l current acc) p reach) f

bfs' :: Machine -> BFSState -> BFSState
bfs' machine (BFS v q p f)
    | f = BFS v q p f
    | L.null q = BFS v q p False
    | otherwise = bfs' machine $ bfsStep machine (BFS v q p f)

steps :: Int -> BFSState
steps 0 = startBFSState 0
steps i = bfsStep m2 $ steps (i-1)

bfsSolution :: BFSState -> Lights -> String
bfsSolution bfs l
    | Map.member l p = show l ++ "->" ++ bfsSolution bfs (p Map.! l)
    | otherwise = show l
    where
        BFS _ _ p _ = bfs

bfsPath :: BFSState -> Lights ->  Lights -> [Lights]
bfsPath bfs start l
    | start == l = [l]
    | Map.member l p = l:bfsPath bfs start (p Map.! l)
    | otherwise = [l]
    where
        (BFS _ _ p _) = bfs

reachable :: [Button] -> Lights -> [Lights]
reachable btns l = [b l | b <- btns]

r' :: Lights -> [Lights]
r' l = reachable btns l
    where
        (_, _, btns) = m1

shortestPath :: Machine -> Lights -> [Lights]
shortestPath m start = bfsPath bfsResult start target
    where
        (_, target, _) = m
        bfsResult = bfs' m (startBFSState start)

findShortest :: Machine -> Distance Int
findShortest m = Infinity

m1 = parseMachine "[.##.] (3) (1,3) (2) (2,3) (0,2) (0,1) {3,5,4,7}"
b6 :: Button
(c1, l1, [b1, b2, b3, b4, b5, b6]) = m1

m2 = parseMachine "[...#.] (0,2,3,4) (2,3) (0,4) (0,1,2) (1,2,3,4) {7,5,12,7,2}"

p2 = parseMachine "[#..#.#..##] (2,8) (1,4,6) (0,1,2,3,7,8,9) (0,1,3,4,5,6,8,9) (0,1,2,3,4,6,7) (2,3,5,6,8,9) (0,1,2,4,5,7,9) (0,1,4) {67,81,56,42,62,16,37,46,31,35}"