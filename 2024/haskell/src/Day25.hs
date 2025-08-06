module Day25 (day25a, day25b) where

import Data.List.Split (splitOn)
import Data.List (transpose)

type Key = [Int]
type Lock = [Int]

{------------------------------{ 1st part }------------------------------}

day25a :: String -> String
day25a = show . part1 . parseLines

parseLines :: String -> ([Key], [Lock])
parseLines line = res 
  where
    blocks = splitOn "\n\n" line
    blocks' = map lines blocks
    (keys, locks) = foldr (\block (k, l) -> if isKey block then (block:k, l) else (k, block:l)) ([], []) blocks'
    res = parseThings (keys, locks)

isKey :: [String] -> Bool
isKey (['#','#','#','#','#']:_) = False
isKey _                         = True

parseThings :: ([[String]], [[String]]) -> ([Key], [Lock])
parseThings (ks, ls) = (ks', ls')
  where
    ks' = map (countHeight . init . tail) ks 
    ls' = map (countHeight . init . tail) ls

countHeight :: [String] -> [Int]
countHeight str = map (sum . map (\y -> if y == '#' then 1 else 0)) $ transpose str

part1 :: ([Key], [Lock]) -> Int
part1 (ks, ls) = length $ filter id $ [keyFit k l | k <- ks, l <- ls]

keyFit :: Key -> Lock -> Bool
keyFit k l = all (<=5) $ zipWith (+) k l

{------------------------------{ 2nd part }------------------------------}

day25b :: String -> String
day25b _ = "Merry christmas"
