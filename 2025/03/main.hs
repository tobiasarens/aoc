import Data.List.Split
import Data.List
import Data.Maybe

main :: IO()
main = long 2

run :: String -> IO()
run file = do
    putStrLn "Part 1"
    ls <-  lines <$> readFile file
    let joltages = map (joltage . stringToDigits) ls
    --print ls
    --print joltages
    let s = sum joltages
    print s
    return ()

run2 :: String -> IO()
run2 file = do
    putStrLn "Part 2"
    ls <- lines <$> readFile file
    let n = 12
    let joltages = map (joltageN 12 . stringToDigits) ls
    --print ls
    --print joltages
    let s = sum joltages
    print s
    return ()
short :: Int -> IO()
short part
    | part == 1 = run "s.txt"
    | otherwise = run2 "s.txt"

long :: Int ->  IO()
long part
    | part == 1 = run "l.txt"
    | otherwise = run2 "l.txt"

maxPair :: Ord a => [a] -> (a, Int)
maxPair list = (max, idx)
    where
        max = maximum list
        idx = fromJust $ elemIndex max list

dropEnd :: Int -> [a] -> [a]
dropEnd n = reverse . drop n . reverse

stringToDigits :: [Char] -> [Int]
stringToDigits [] = []
stringToDigits (c:cs) = d : stringToDigits cs
    where
        d = read (c : "") :: Int


joltage :: [Int] -> Int
joltage ls = read (show first ++ show second )
    where
        (first, idx) = maxPair $ dropEnd 1 ls
        second = maximum $ drop (idx + 1) ls

joltageN :: Int -> [Int] -> Int
joltageN 0 _ = 0
joltageN 1 ls = maximum ls
joltageN n ls = read (show curr ++ show (joltageN (n-1) remain))
    where
        (curr, idx) = maxPair $ dropEnd (n-1) ls
        remain = drop (idx + 1) ls