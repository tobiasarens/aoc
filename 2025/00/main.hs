import Data.List.Split

main :: IO()
main = long 2

run :: String -> IO()
run file = do
    putStrLn "Part 1"
    ls <-  lines <$> readFile file
    print ls
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