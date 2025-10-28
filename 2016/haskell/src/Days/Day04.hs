module Days.Day04 (day04a, day04b) where

{------------------------------{ 1st part }------------------------------}

day04a :: String -> String
day04a = show . part1 . parseLines . lines

parseLines :: [String] -> [[Int]]
parseLines = undefined

part1 :: [[Int]] -> Int
part1 = undefined

{------------------------------{ 2nd part }------------------------------}

day04b :: String -> String
day04b = show . part2 . parseLines . lines

part2 :: [[Int]] -> Int
part2 = undefined
