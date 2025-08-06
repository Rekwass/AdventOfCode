module Day09 (day09a, day09b) where
import Data.Char (ord)
import Data.Maybe (catMaybes, isNothing, fromJust)
import Data.Foldable (foldl')

type File = (Size, Id)
type Size = Int
type Id = Int

{------------------------------{ 1st part }------------------------------}

day09a :: String -> String
day09a = show . part1 . parseLine

parseLine :: String -> [Maybe File]
parseLine ls = go ls 0
    where
      go [] _ = []
      go [x] i = replicate (toNum x) (Just (toNum x, i))
      go (x:y:ys) i = replicate (toNum x) (Just (toNum x, i)) ++ replicate (toNum y) Nothing ++ go ys (i + 1)
      toNum = flip (-) 48 . ord

part1 :: [Maybe File] -> Int
part1 fs = computeRes 0 $ take (length files) $ fillGap fs (reverse files)
    where
      files = catMaybes fs

fillGap :: [Maybe File] -> [File] -> [File]
fillGap [] _ = []
fillGap _ [] = []
fillGap (x:xs) (y:ys)
    | isNothing x = y : fillGap xs ys
    | otherwise = fromJust x : fillGap xs (y:ys)

computeRes :: Int -> [File] -> Int
computeRes _ [] = 0
computeRes p ((_, i):fs) = p * i + computeRes (p + 1) fs

{------------------------------{ 2nd part }------------------------------}

day09b :: String -> String
day09b = show . part2 . parseLine'

parseLine' :: String -> [File]
parseLine' ls = go (init ls) 0
    where
      go [] _ = []
      go [x] i = [(toNum x, i)]
      go (x:y:ys) i = (toNum x, i) : (toNum y, -1) : go ys (i + 1)
      toNum = flip (-) 48 . ord

part2 :: [File] -> Int
part2 fs = compute 0 $ convertToIds $ foldl' attemptMoveBlock fs noEmptyRev
    where
      noEmptyRev = reverse $ filter (\(_, i) -> i /= -1) fs

convertToIds :: [File] -> [Id]
convertToIds [] = []
convertToIds ((s,i):fs) = replicate s i ++ convertToIds fs

compute :: Int -> [Id] -> Int
compute _ [] = 0
compute p (-1:is) = compute (p + 1) is
compute p (i:is) = p * i + compute (p + 1) is

attemptMoveBlock :: [File] -> File -> [File]
attemptMoveBlock [] _ = []
attemptMoveBlock (x@(s1, i1):xs) f@(s2, i2)
    | i1 == i2 = x:xs
    | i1 /= -1 = doNothing
    | s1 == s2 = f : (s1 - s2, -1) : noBlock
    | s1 > s2 = f : (s1 - s2, -1) : noBlock
    | otherwise = doNothing
    where
      noBlock = removeBlockById xs i2
      doNothing = x : attemptMoveBlock xs f

removeBlockById :: [File] -> Id -> [File]
removeBlockById [] _ = []
removeBlockById (f@(s1, i1):fs) i2
    | i1 == i2 = (s1, -1) : fs
    | otherwise = f : removeBlockById fs i2
