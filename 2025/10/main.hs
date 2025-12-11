import Data.List.Split
import Data.Bits
import Data.List (foldl')
import qualified Data.List as L
import qualified Data.Maybe as Maybe
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map (Map)
import qualified Data.Map as Map

import Data.PQueue.Prio.Min (MinPQueue)
import qualified Data.PQueue.Prio.Min as PQ

main :: IO()
main = long 2

run :: String -> IO()
run file = do
    putStrLn "Part 1"
    ls <-  lines <$> readFile file
    -- let machines = map parseMachine ls
    -- let shortest = map shortestSol machines
    -- print $ sum shortest

    return ()

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


solveMachine :: Machine -> Int
solveMachine _ = 0

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

data SearchState = SearchState {
    visited :: Set Lights,
    prec :: Map Lights (Lights, Int),
    distances :: Map Lights (Distance Int),
    queue :: MinPQueue (Distance Int) Lights
}

instance Show SearchState where
    show (SearchState v p d q) = "visited: " ++ (show v) ++ "\n" ++ "distances: " ++ (show d) ++ "\n" ++ "precs: " ++ (show p) ++ "\n"

emptySearchState = SearchState Set.empty Map.empty Map.empty PQ.empty
startSearchState light = SearchState (Set.singleton light) (Map.singleton light (0, 0)) (Map.singleton light (Dist 0)) PQ.empty

reachable :: Machine -> Lights -> [(Lights, Int)]
reachable (_, _, btns) light = [((btns !! i) light, i) | i <-[0..length btns - 1]]

traverse :: Machine -> SearchState -> Lights -> SearchState
traverse m ss l = ss

updateState :: Lights -> (Lights, Int) -> SearchState -> SearchState 
updateState origin (light, withBtn) (SearchState v p d q) 
    | Set.member light v = SearchState v p' d' q
    | otherwise = SearchState (Set.insert light v) (Map.insert light (origin, withBtn) p ) (Map.insert light (addDist (d Map.! origin) (Dist 1)) d) q
    where
        currDist = d Map.! light
        originDist = d Map.! origin

        (p', d') = if currDist >= originDist
                then (p, d)
                else (Map.insert light (origin, withBtn) p, Map.insert light (addDist originDist (Dist 1)) d)


exploreAll :: Machine -> Lights -> SearchState -> SearchState
exploreAll (c, target, btns) pos (SearchState v p d q) 
    | pos == target = keepMin (SearchState v p d q) 
    | Set.member pos v = SearchState v p d q

shortestPath :: Machine -> Lights -> Lights -> [Int]
shortestPath m start stop = []

findShortest :: Machine -> Distance Int
findShortest m = Infinity

m1 = parseMachine "[.##.] (3) (1,3) (2) (2,3) (0,2) (0,1) {3,5,4,7}"
b6 :: Button
(c1, l1, [b1, b2, b3, b4, b5, b6]) = m1

m2 = parseMachine "[#..#.#..##] (2,8) (1,4,6) (0,1,2,3,7,8,9) (0,1,3,4,5,6,8,9) (0,1,2,3,4,6,7) (2,3,5,6,8,9) (0,1,2,4,5,7,9) (0,1,4) {67,81,56,42,62,16,37,46,31,35}"