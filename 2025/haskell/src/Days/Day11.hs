{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Days.Day11 (day11a, day11b) where

import qualified Data.Map as M
import Data.List.Split (splitOn)
import Data.MemoTrie (memo2)

{------------------------------{ 1st part }------------------------------}

day11a :: String -> String
day11a = show . part1 . parseLines . lines

parseLines :: [String] -> M.Map String [String]
parseLines = M.fromList . map (\l -> let [x, xs] = splitOn ": " l in (x, words xs))

part1 :: M.Map String [String] -> Int
part1 m = countPaths (m M.! "you")
  where
    countPaths = sum . map go
    go v = if v == "out" then 1 else countPaths (m  M.! v)
    
{------------------------------{ 2nd part }------------------------------}

day11b :: String -> String
day11b = show . part2 . parseLines . lines

part2 :: M.Map String [String] -> Int
part2 m = f 0 (m M.! "svr")
  where
    (f, g) = (memo2 countPaths, memo2 go)
    countPaths :: Int -> [String] -> Int
    countPaths i = sum . map (g i)
    go i v
      | v == "out"              = if i < 2 then 0 else 1
      | v `elem` ["dac", "fft"] = f (i + 1) (m M.! v)
      | otherwise               = f i       (m M.! v)
