module Days.Day06 (day06a, day06b) where

import Data.List (transpose, sort, group, maximumBy, minimumBy)
import Data.Function (on)

{------------------------------{ 1st part }------------------------------}

day06a :: String -> String
day06a = show . part1 . lines

part1 :: [String] -> String
part1 = map ((head . maximumBy (compare `on` length)) . group . sort) . transpose

{------------------------------{ 2nd part }------------------------------}

day06b :: String -> String
day06b = show . part2 . lines

part2 :: [String] -> String
part2 = map ((head . minimumBy (compare `on` length)) . group . sort) . transpose
