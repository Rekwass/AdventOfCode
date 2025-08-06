module Day10 (day10a, day10b) where

day10a :: String -> String
day10a = show . run . head . lines

run :: String -> Int
run x = length $ iterate lookAndSay x !! 40

lookAndSay :: String -> String
lookAndSay [] = []
lookAndSay s@(x:_) = start ++ lookAndSay e
    where
      (b, e) = span (== x) s
      start = show (length b) ++ [x]

---------------------------------------------------------------------------
-- Part 2.

day10b :: String -> String
day10b = show . run' . head . lines

run' :: String -> Int
run' x = length $ iterate lookAndSay x !! 50
