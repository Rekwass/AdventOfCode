{-# OPTIONS_GHC -Wno-incomplete-uni-patterns -Wno-x-partial #-}

module Days.Day02 (day02a, day02b) where
import Data.List.Split (splitOn, chunksOf)

{------------------------------{ 1st part }------------------------------}

day02a :: String -> String
day02a = show . part1 . parseLine . init

parseLine :: String -> [[Int]]
parseLine = map (\s -> let [x,y] = map read $ splitOn "-" s in [x..y]) . splitOn ","

part1 :: [[Int]] -> Int
part1 = sum . concatMap (filter isValid)

isValid :: Int -> Bool
isValid x = uncurry (==) $ splitAt (length s `div` 2) s
  where
    s = show x

{------------------------------{ 2nd part }------------------------------}

day02b :: String -> String
day02b = show . part2 . parseLine . init

part2 :: [[Int]] -> Int
part2 = sum . concatMap (filter isValid')

isValid' :: Int -> Bool
isValid' n = go (l `div` 2)
  where
    s = show n
    l = length s
    go 0 = False
    go i 
      | let xs = chunksOf i s in all (== head xs) (tail xs) = True
      | otherwise = go (i - 1)
