module Day17 (day17a, day17b) where
import Data.List (subsequences, sort, group)

{------------------------------{ 1st part }------------------------------}

day17a :: String -> String
day17a = show . run . parseLines . lines

parseLines :: [String] -> [Int]
parseLines = map read

run :: [Int] -> Int
run = length . filter id . map ((==150) . sum) . subsequences

{------------------------------{ 2nd part }------------------------------}

day17b :: String -> String
day17b = show . run' . parseLines . lines

run' :: [Int] -> Int 
run' xs = head . map length . group . sort . map (length . snd) . filter fst $ zip (map ((==150) . sum) btls) btls
    where
      btls = tail $ subsequences xs
