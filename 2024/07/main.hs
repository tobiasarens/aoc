import Data.List.Split

main :: IO()
main = do
    putStrLn "running part 1 on long input"
    long 1
    putStrLn ""
    putStrLn "running part 2 on long input"
    long 2

run :: String -> IO()
run file = do
    putStrLn "Part 1"
    ls <- map (splitOn ":") . lines <$> readFile file
    let problems = map (\l -> (readInt (head l) , map readInt (words (l !! 1) ))) ls :: [(Int, [Int])]
    --print problems

    let possible = filter solvable problems

    --print possible

    print . sum $ map fst possible

    return ()

run2 :: String -> IO()
run2 file = do
    putStrLn "Part 2"
    ls <- map (splitOn ":") . lines <$> readFile file
    let problems = map (\l -> (readInt (head l) , map readInt (words (l !! 1) ))) ls :: [(Int, [Int])]
    --print problems

    let possible = filter solvable2 problems

    --print possible

    print . sum $ map fst possible
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

data Operator = PLUS | MUL
    deriving (Eq, Show, Enum)


solvable :: (Int, [Int]) -> Bool
solvable (target ,[] )= False
solvable (target ,[a]) = target == a
solvable (target, one:two:tl) = (one * two <= target && solvable (target, (one * two):tl))
                            || (one + two <= target && solvable (target ,(one + two):tl))

concatInt :: Int -> Int -> Int
concatInt a b = read (show a ++ show b)

solvable2 :: (Int, [Int]) -> Bool
--solvable2 (target ,[] )= False
solvable2 (target ,[a]) = target == a
solvable2 (target, one:two:tl) =
                            (concatInt one two <= target && solvable2 (target, concatInt one two:tl))
                            || (one * two <= target && solvable2 (target, (one * two):tl))
                            || (one + two <= target && solvable2 (target ,(one + two):tl))

