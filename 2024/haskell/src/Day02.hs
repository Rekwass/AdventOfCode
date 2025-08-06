{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Day02 (day02a, day02b) where
import Data.List.Split (splitOn)

type Path = [Int]

{------------------------------{ 1st part }------------------------------}

day02a :: String -> String
day02a = show . part1 . parseLines . lines

parseLines :: [String] -> [Path]
parseLines = map (map read . splitOn " ")

part1 :: [Path] -> Int
part1 = length . filter (\x -> ascending x || descending x)

ascending :: Path -> Bool
ascending (x:y:xs) = y - x `elem` [1,2,3] && ascending (y:xs)
ascending _ = True

descending :: Path -> Bool
descending (x:y:xs) = x - y `elem` [1,2,3] && descending (y:xs)
descending _ = True

{------------------------------{ 2nd part }------------------------------}

day02b :: String -> String
day02b = show . part2 . parseLines . lines

part2 :: [Path] -> Int
part2 = length . filter (\x -> ascending' x [] || descending' x [])

ascending' :: Path -> Path -> Bool
ascending' (x:y:xs) ys
    | y - x `elem` [1,2,3] = ascending' (y:xs) (ys ++ [x])
    | otherwise = ascending (ys ++ x:xs) || ascending (ys ++ y:xs)
ascending' _ _ = True

descending' :: Path -> Path -> Bool
descending' (x:y:xs) ys
    | x - y `elem` [1,2,3] = descending' (y:xs) (ys ++ [x])
    | otherwise = descending (ys ++ x:xs) || descending (ys ++ y:xs)
descending' _ _ = True
