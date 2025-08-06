module Day11 (day11a, day11b) where

import qualified Data.IntMultiSet as MS

{------------------------------{ 1st part }------------------------------}

day11a :: String -> String
day11a = show . part1 . parseLine

parseLine :: String -> [Int]
parseLine = map read . words

part1 :: [Int] -> Int
part1 xs = length $ iterate (concatMap blink) xs !! 25

blink :: Int -> [Int]
blink 0 = [1]
blink x
    | even numberLen = [l, r]
    | otherwise = [x * 2024]
    where
      numberLen = length $ show x
      (l, r) = divMod x (10 ^ (numberLen `div` 2))

{------------------------------{ 2nd part }------------------------------}

day11b :: String -> String
day11b = show . part2 . parseLine

part2 :: [Int] -> Int
part2 xs = MS.size $ iterate (MS.concatMap blink) msxs !! 75
    where msxs = MS.fromList xs
