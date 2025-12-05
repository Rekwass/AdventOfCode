module Days.Day03 (day03a, day03b) where

import Data.List (elemIndex)
import Data.Char (digitToInt)
import Data.Maybe (fromJust)

{------------------------------{ 1st part }------------------------------}

day03a :: String -> String
day03a = show . part1 . parseLines . lines

parseLines :: [String] -> [[Int]]
parseLines = map (map digitToInt)

part1 :: [[Int]] -> Int
part1 = sum . map bestJoltage2

bestJoltage2 :: [Int] -> Int
bestJoltage2 xs = m1 * 10 + m2
  where
    m1 = maximum (init xs)
    idx = fromJust $ elemIndex m1 xs
    m2 = maximum (drop (idx + 1) xs)

{------------------------------{ 2nd part }------------------------------}

day03b :: String -> String
day03b = show . part2 . parseLines . lines

part2 :: [[Int]] -> Int
part2 = sum . map bestJoltage12

bestJoltage12 :: [Int] -> Int
bestJoltage12 xs = go 11 x rest
  where
    (x, rest) = pickMax xs 11

pickMax :: [Int] -> Int -> (Int, [Int])
pickMax xs k =
  let x   = maximum (take k xs)
      idx = fromJust (elemIndex x xs)
  in (x, drop (idx + 1) xs)

go :: Int -> Int -> [Int] -> Int
go 0 acc _  = acc
go n acc xs = acc * 10 ^ n + go (n - 1) x rest
  where
    (x, rest) = pickMax xs (length xs - (n - 1))
