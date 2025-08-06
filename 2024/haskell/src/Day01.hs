{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
module Day01 (day01a, day01b) where

import Data.List.Split (splitOn)
import Data.List (sort)
import Control.Arrow ((***))

{------------------------------{ 1st part }------------------------------}

day01a :: String -> String
day01a = show . part1 . (sort *** sort) . parseLines . lines

parseLines :: [String] -> ([Int], [Int])
parseLines [] = ([], [])
parseLines (l:ls) = (x : xs, y : ys)
    where
      [x,y] = map read $ splitOn "   " l :: [Int]
      (xs, ys) = parseLines ls

part1 :: ([Int], [Int]) -> Int
part1 (xs, ys) = foldr (\(x,y) acc -> abs (x - y) + acc) 0 $ zip xs ys


{------------------------------{ 2nd part }------------------------------}

day01b :: String -> String
day01b = show . part2 . parseLines . lines

part2 :: ([Int], [Int]) -> Int
part2 (xs, ys) = foldr (\x acc -> x * length (filter (==x) ys) + acc) 0 xs
