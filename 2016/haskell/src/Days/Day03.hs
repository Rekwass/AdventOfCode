{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Days.Day03 (day03a, day03b) where

import Data.List (transpose)
import Data.List.Split (chunksOf)

{------------------------------{ 1st part }------------------------------}

day03a :: String -> String
day03a = show . part1 . parseLines . lines

parseLines :: [String] -> [[Int]]
parseLines = map (map read . words)

part1 :: [[Int]] -> Int
part1 = length . filter id . map isPossible

isPossible :: [Int] -> Bool
isPossible [a,b,c] = a + b > c && a + c > b && b + c > a

{------------------------------{ 2nd part }------------------------------}

day03b :: String -> String
day03b = show . part2 . parseLines . lines

part2 :: [[Int]] -> Int
part2 = length . filter id . map isPossible . concatMap transpose . chunksOf 3
