{-# OPTIONS_GHC -Wno-incomplete-uni-patterns -Wno-x-partial #-}

module Days.Day05 (day05a, day05b) where

import Data.List.Split (splitOn)
import Data.List (groupBy, sortOn)

{------------------------------{ 1st part }------------------------------}

type Id = Int
type Range = (Id, Id)

day05a :: String -> String
day05a = show . part1 . parseLines

parseLines :: String -> ([Range], [Id])
parseLines line = (parseRanges $ lines rs, map read $ lines xs)
  where
    [rs, xs] = splitOn "\n\n" line

parseRanges :: [String] -> [Range]
parseRanges = map (\x -> let [l, r] = splitOn "-" x in (read l,read r))

part1 :: ([Range], [Id]) -> Int
part1 (rs, xs) = length . filter (`isFresh` rs) $ xs

isFresh :: Id -> [Range] -> Bool
isFresh x = any (\(l, h) -> x >= l && x <= h)

{------------------------------{ 2nd part }------------------------------}

day05b :: String -> String
day05b = show . part2 . parseLines

part2 :: ([Range], [Id]) -> Int
part2 (rs, _) = sum [h - l + 1 | (l, h) <- rs']
  where
    rs' = fst . head . dropWhile (uncurry (/=)) $ zip steps (tail steps)
    steps = iterate step rs

step :: [Range] -> [Range]
step = map mergeRanges . groupBy doOverlap . sortOn fst

doOverlap :: Range -> Range -> Bool
doOverlap (l1, h1) (l2, h2) = not (h1 < l2 || h2 < l1)

mergeRanges :: [Range] -> Range
mergeRanges xs = (minimum l, maximum h)
  where
    l = map fst xs
    h = map snd xs
