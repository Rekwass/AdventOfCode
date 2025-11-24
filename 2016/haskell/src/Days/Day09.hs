{-# LANGUAGE OverloadedStrings #-}

module Days.Day09 (day09a, day09b) where

import Data.List (elemIndex)
import qualified Data.Text as T
import Data.Maybe (fromJust)
import Data.List.Split (splitOn)

{------------------------------{ 1st part }------------------------------}

day09a :: String -> String
day09a = show . part1

part1 :: String -> Int
part1 = length . decompress . T.unpack . T.strip . T.pack

decompress :: String -> String
decompress [] = []
decompress ('(':xs) = concat (replicate n (take l rest)) ++ decompress (drop l rest)
  where
    (nums, _:rest) = splitAt (fromJust $ elemIndex ')' xs) xs
    [l, n] = map read $ splitOn "x" nums
decompress (x:xs) = x : decompress xs

{------------------------------{ 2nd part }------------------------------}

day09b :: String -> String
day09b = show . part2

part2 :: String -> Int
part2 = decompress' . T.unpack . T.strip . T.pack

decompress' :: String -> Int
decompress' [] = 0
decompress' ('(':xs) = n * decompress' (take l rest) + decompress' (drop l rest)
  where
    (nums, _:rest) = splitAt (fromJust $ elemIndex ')' xs) xs
    [l, n] = map read $ splitOn "x" nums
decompress' (x:xs) = 1 + decompress' xs
