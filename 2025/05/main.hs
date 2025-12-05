import Data.List.Split
import qualified Data.Set as Set
import Data.Set (Set)
import qualified Data.SortedList as SL
import Data.SortedList (SortedList)
import qualified Control.Applicative as SL
import Data.Monoid
import Data.Maybe

main :: IO()
main = long 2

run :: String -> IO()
run file = do
    putStrLn "Part 1"
    [rangesS, itemsS] <- splitOnEmptyLine . lines <$> readFile file
    let ranges = map parseRange rangesS
    let items = map read itemsS :: [Int]

    --print ranges
    let fresh = filter (isFresh ranges) items
    print $ length fresh
    return ()

run2 :: String -> IO()
run2 file = do
    putStrLn "Part 2"
    [rangesS, itemsS] <- splitOnEmptyLine . lines <$> readFile file
    let ranges = map parseRange rangesS

    let sr = SL.fromSortedList . SL.toSortedList $ ranges

    --print sr
    let sr' = collapse sr
    
    --print sr'

    let s = map toSize sr'
    print $ sum s

    return ()

short :: Int -> IO()
short part
    | part == 1 = run "s.txt"
    | otherwise = run2 "s.txt"

long :: Int ->  IO()
long part
    | part == 1 = run "l.txt"
    | otherwise = run2 "l.txt"

splitOnEmptyLine :: [String] -> [[String]]
splitOnEmptyLine = splitOn [""]

parseRange :: String -> (Int, Int)
parseRange inp = (read f, read s)
    where
        [f, s] = splitOn "-" inp

isIn :: (Int, Int) -> Int -> Bool
isIn (s, e) i = (s <= i) && (e >= i)

isFresh :: [(Int, Int)] -> Int -> Bool
isFresh ranges item = any (`isIn` item) ranges

addToSet ::  (Int, Int) -> Set Int -> Set Int
addToSet (b, e) s  = Set.union s $ Set.fromList [b .. e]

overlap :: (Int, Int) -> (Int, Int) -> Bool
overlap (s1, e1) (s2, e2) = (s1 < s2 && e1 < e2 && e1 >= s2) || (s2 < s1 && e2 < e2 && e2 >= s1)

include :: (Int, Int) -> (Int, Int) -> Bool
include (s1, e1) (s2, e2) = s1 <= s2 && e1 >= e2

spanR :: (Int, Int) -> (Int, Int) -> Maybe (Int, Int)
spanR a b
    | include a b = Just a
    | include b a = Just b
    | overlap a b = Just (min s1 s2, max e1 e2)
    | otherwise = Nothing
        where
            (s1, e1) = a
            (s2, e2) = b

collapse :: [(Int, Int)] -> [(Int, Int)]
collapse [] = []
collapse [a] = [a]
collapse (one:tl)
    | isNothing (spanR one two) = one : two : tl'
    | otherwise = fromJust (spanR one two) : tl'
        where
            (two:tl') = collapse tl

toSize :: (Int, Int) -> Int
toSize (a, b) = b - a + 1