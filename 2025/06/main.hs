import qualified Data.List.Split as SP
import qualified Data.Text as T
import qualified Data.Char as C
main :: IO()
main = long 2

type OP = Int -> Int -> Int

instance Show OP where
    show op = ""

run :: String -> IO()
run file = do
    putStrLn "Part 1"
    (inOp, inNum) <- splitAt 1 . map words . reverse . lines <$> readFile file
    let base = map lineFn $ head inOp
    let numbers = map (map read) inNum :: [[Int]]
    let sols = foldr (flip (zipWith apply)) base numbers

    print . sum $ map snd sols

    return ()

run2 :: String -> IO()
run2 file = do
    putStrLn "Part 2"
    (inOp, inNum) <- splitAt 1 . reverse . lines <$> readFile file

    let base = map lineFn .  words $ head inOp
    let numbers = SP.splitWhen (all C.isSpace) $ transpose (reverse inNum)

    let numbers' =  map (calcBlock . transpose) numbers

    let sols = zipWith applyAll base numbers'

    print . sum $ map snd sols

    return ()

short :: Int -> IO()
short part
    | part == 1 = run "s.txt"
    | otherwise = run2 "s.txt"

long :: Int ->  IO()
long part
    | part == 1 = run "l.txt"
    | otherwise = run2 "l.txt"

lineFn :: String -> (OP, Int)
lineFn s
    | strip s == "+" = ((+), 0)
    | strip s == "*" = ((*), 1)
    | otherwise = (\a b ->  0, 0)

strip :: String -> String
strip = T.unpack . T.strip . T.pack

apply :: (OP, Int) -> Int -> (OP, Int)
apply (op, a) b = (op, op a b)

applyAll :: (OP, Int) -> [Int] -> (OP, Int)
applyAll op  [] = op
applyAll op [i] = apply op i
applyAll op (h:t) = applyAll (apply op h) t

transpose :: [[a]] -> [[a]]
transpose [] = []
transpose [a] = map (: []) a
transpose (h:ts) = zipWith (:) h $ transpose ts

calcBlock :: [String] -> [Int]
calcBlock = map read . transpose