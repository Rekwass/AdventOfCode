{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Day07 (day07a, day07b) where

{------------------------------{ 1st part }------------------------------}

day07a :: String -> String
day07a = show . part1 . parseLines . lines

parseLines :: [String] -> [(Int, [Int])]
parseLines = map ((\l -> (read $ init $ head l, map read (tail l))) . words)

part1 :: [(Int, [Int])] -> Int
part1 = sum . map fst . filter (isComputable 0)

isComputable :: Int -> (Int, [Int]) -> Bool
isComputable acc (total, [x]) = acc + x == total || acc * x == total
isComputable acc (total, x:xs) = isComputable (acc + x) (total, xs) || isComputable (acc * x) (total, xs)

{------------------------------{ 2nd part }------------------------------}

day07b :: String -> String
day07b = show . part2 . parseLines . lines

part2 :: [(Int, [Int])] -> Int
part2 = sum . map fst . filter (isComputable' 0)

isComputable' :: Int -> (Int, [Int]) -> Bool
isComputable' acc (total, [x]) = acc + x == total || acc * x == total || acc * (10 ^ intLength x) + x == total
isComputable' acc (total, x:xs) = isComputable' (acc + x) (total, xs) || isComputable' (acc * x) (total, xs) || isComputable' (acc * (10 ^ intLength x) + x) (total, xs)

intLength :: Int -> Int
intLength = length . show
