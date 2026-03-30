{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -Wno-x-partial #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Days.Day20 (day20a, day20b) where
import Data.List.Split (splitOn)
import Data.List (sortBy)
import Data.Function (on)

{------------------------------{ 1st part }------------------------------}

type Range = (Int, Int)

day20a :: String -> String
day20a = show . part1 . parseLines . lines

parseLines :: [String] -> [Range]
parseLines = sortBy (compare `on` fst) . map ((\[l, r] -> (read l, read r)) . splitOn "-")

part1 :: [Range] -> Int
part1 rs = snd (head $ mergeRanges rs) + 1

mergeRanges :: [Range] -> [Range]
mergeRanges [] = []
mergeRanges [ra1@(l1, r1), ra2@(l2, r2)]
  | r1 < (l2 - 1) = [ra1, ra2]
  | r1 > r2       = [ra1]
  | otherwise     = [(l1, r2)]
mergeRanges (ra1@(l1, r1):ra2@(l2, r2):rs)
  | r1 < (l2 - 1) = ra1 : mergeRanges (ra2:rs)
  | r1 > r2       = mergeRanges (ra1:rs)
  | otherwise     = mergeRanges ((l1, r2):rs)

{------------------------------{ 2nd part }------------------------------}

day20b :: String -> String
day20b = show . part2 . parseLines . lines

part2 :: [Range] -> Int
part2 rs = countIps rs'
  where
    rs' = mergeRanges rs

countIps :: [Range] -> Int
countIps [] = 0
countIps ((_, r1):ra2@(l2,_):rs) = l2 - (r1 + 1) + countIps (ra2:rs)
countIps _ = 0
