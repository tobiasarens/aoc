
readFileLineByLine ::  String -> IO([String])
readFileLineByLine filename = do
    content <- readFile filename
    return (lines content)

inputFile = "l.txt"


data Dir a = Left | Right deriving Show

main :: IO()
main = do
    putStrLn "Hello World"
    inputLines <- readFileLineByLine inputFile
    let initial = (0, 50)
    let commands = parseList inputLines
    --print $ take 5 commands
    let res = foldl move initial commands
    print res
    return ()

left :: (Int,Int) -> Int -> (Int, Int)
left (counter, 0) move = left (counter, 100) move
left (counter, pos) move = increase (counter, pos - move)
    where
        increase :: (Int, Int) -> (Int, Int)
        increase (c, val)
            | val > 0 = (c, val)
            | val == 0 = (c + 1, val)
            | val < 0 = increase (c +1, val+100)

right :: (Int, Int) -> Int -> (Int, Int)
right (counter, pos) move = decrease (counter, pos + move)
    where
        decrease :: (Int, Int) -> (Int, Int)
        decrease (c, val)
            | val < 100 = (c, val)
            | val == 100 = (c+1, 0)
            | val > 100 = decrease (c +1, val - 100)


move :: (Int, Int) -> (Dir a, Int) -> (Int, Int)
move p (Main.Left, m) = left p m
move p (Main.Right, m) = right p m

parseList :: [String] -> [(Dir a, Int)]
parseList [] = []
parseList (s:ss) = parseElem s : parseList ss
    where
        parseElem :: String -> (Dir a, Int)
        parseElem ('R':cs) = (Main.Right, read cs)
        parseElem ('L':cs) = (Main.Left, read cs)