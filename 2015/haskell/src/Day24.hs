module Day24 (day24a, day24b) where

{------------------------------{ 1st part }------------------------------}

day24a :: String -> String
day24a = show . part1 . parseLines . lines

parseLines :: [String] -> [Int]
parseLines = map read

part1 :: [Int] -> Int
part1 = quantumEntanglement 3

combinations :: [a] -> Int -> [[a]]
combinations _  0 = [[]]
combinations [] _ = []
combinations (x:xs) k
  | k < 0     = []
  | otherwise = map (x:) (combinations xs (k-1)) ++ combinations xs k

quantumEntanglement :: Int -> [Int]  -> Int
quantumEntanglement nbGroups xs = minimum . head . dropWhile null $ [ [product c | c <- combinations xs n, sum c == maxWeight] | n <- [1 .. length xs]]
  where
    maxWeight = sum xs `div` nbGroups

{------------------------------{ 2nd part }------------------------------}

day24b :: String -> String
day24b = show . part2 . parseLines . lines

part2 :: [Int] -> Int
part2 = quantumEntanglement 4
