{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -Wno-x-partial #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Day25 (day25a, day25b) where

import Text.Regex.PCRE.Heavy (re, scan)
import Data.Maybe (fromJust)
import Data.List (elemIndex)

{------------------------------{ 1st part }------------------------------}

day25a :: String -> String
day25a = show . part1 . parseLine

parseLine :: String -> (Int, Int)
parseLine = (\[r, c] -> (r, c)) . map read . snd . head . scan [re|row (\d+), column (\d+)|]

part1 :: (Int, Int) -> Int
part1 coord = iterate computeNext 20151125 !! idx
  where
    idx = fromJust $ elemIndex coord coords

computeNext :: Int -> Int
computeNext n = (n * 252533) `mod` 33554393

-- >>> take 5 coords
-- [(1,1),(2,1),(1,2),(3,1),(2,2)]
coords :: [(Int, Int)]
coords = [(i, j) | n <- [2..], i <- [n - 1, n-2..1], let j = n - i]

{------------------------------{ 2nd part }------------------------------}

day25b :: String -> String
day25b = show . part2 . parseLine

part2 :: (Int, Int) -> String
part2 _ = "No part 2"
