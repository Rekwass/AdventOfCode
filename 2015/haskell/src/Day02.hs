module Day02 (day02a, day02b) where

import Data.List.Split

day02a :: String -> String
day02a x = show . sum $ map getTotalPaper $ lines x

day02b :: String -> String
day02b x = show . sum $ map getTotalRibbon $ lines x

getTotalPaper :: String -> Int
getTotalPaper x  = getTotalPaper' $ map read $ splitOn "x" x

getTotalPaper' :: [Int] -> Int
getTotalPaper' [] = 0
getTotalPaper' [_]       = 0
getTotalPaper' [_, _]    = 0
getTotalPaper' (l:w:h:_) = 2 * l * w + 2 * w * h + 2 * l * h + minimum [l * w, w * h, l * h]

getTotalRibbon :: String -> Int
getTotalRibbon x  = getTotalRibbon' $ map read $ splitOn "x" x

getTotalRibbon' :: [Int] -> Int
getTotalRibbon' [] = 0
getTotalRibbon' [_]       = 0
getTotalRibbon' [_, _]    = 0
getTotalRibbon' (l:w:h:_) = l * w * h + 2 * minimum [l + w, w + h, l + h]
