{-# LANGUAGE QuasiQuotes #-}
module Day03 (day03a, day03b) where

import Text.Regex.PCRE.Heavy (re, scan)

{------------------------------{ 1st part }------------------------------}

day03a :: String -> String
day03a = show . part1 . parseLine

parseLine :: String -> [[Int]]
-- INFO:                                   Regex with QuasiQuotes. Using PCRE
parseLine = map (map read . snd) . scan [re|mul\(([0-9]{1,3}),([0-9]{1,3})\)|]

part1 :: [[Int]] -> Int
part1 = sum . map product

{------------------------------{ 2nd part }------------------------------}

day03b :: String -> String
day03b = show . part2 . parseLine'

parseLine' :: String -> [[Int]]
parseLine' line = map (map read) $ toggleInstr result True
    where
      result = scan [re|mul\(([0-9]{1,3}),([0-9]{1,3})\)|do\(\)|don't\(\)|] line

toggleInstr :: [(String, [String])] -> Bool -> [[String]]
toggleInstr []                  _     = []
toggleInstr (("do()", _):xs)    _     = toggleInstr xs True
toggleInstr (("don't()", _):xs) _     = toggleInstr xs False
toggleInstr ((_,x):xs)          True  = x : toggleInstr xs True
toggleInstr (_:xs)              False = toggleInstr xs False

part2 :: [[Int]] -> Int
part2 = sum . map product
