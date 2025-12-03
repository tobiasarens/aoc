import Data.List.Split

type ID a = String

main :: IO()
main = long 2

run :: String -> IO()
run file = do
    ls <- splitOn "," . concat . lines <$> readFile file
    let ranges = map splitToRange ls :: [(Int, Int)]
    let invalids = map (uncurry checkRange) ranges :: [[Int]]
    let s = sum $ map sum invalids
    print s
    return ()

run2 :: String -> IO()
run2 file = do
    putStrLn "Part 2"
    ls <- splitOn "," . concat . lines <$> readFile file
    let ranges = map splitToRange ls :: [(Int, Int)]
    let invalids = map (uncurry checkRangePt2) ranges :: [[Int]]
    let s = sum $ map sum invalids
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

checkNumber :: String -> Bool
checkNumber s
    | mod (length s) 2 == 1 = False
    | otherwise = hlf1 == hlf2
        where
            (hlf1, hlf2) = splitAt (div (length s) 2) s

untuple :: (a, a) -> [a]
untuple (a, b) = [a, b]

slice :: Int -> String -> [String]
slice _ [] = []
slice i s = part : slice i remain
    where
        (part, remain) = splitAt i s

check :: Int -> String -> Bool
check l s
    | (l * 2) > (length s) = False
    | otherwise = (check (l+1) s) || (all ((ss!!0)==) ss)
        where
            ss = slice l s

checkNumberPt2 :: String -> Bool
checkNumberPt2 = check 1


checkRangePt2 :: Int-> Int -> [Int]
checkRangePt2 a b
    | a == b = [a | checkNumberPt2 $ show a]
    | otherwise = if checkNumberPt2 $ show a
        then a:checkRangePt2 (a+1) b
        else checkRangePt2 (a+1) b

checkRange :: Int-> Int -> [Int]
checkRange a b
    | a == b = [a | checkNumber $ show a]
    | otherwise = if checkNumber $ show a
        then a:checkRange (a+1) b
        else checkRange (a+1) b

splitToRange :: String -> (Int, Int)
splitToRange s = (start, end)
    where
        readInt :: String -> Int
        readInt = read
        [start, end] = map readInt $ splitOn "-" s