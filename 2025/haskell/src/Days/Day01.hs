{-# LANGUAGE LambdaCase #-}

module Days.Day01 (day01a, day01b) where

import Data.List (foldl')

{------------------------------{ 1st part }------------------------------}

day01a :: String -> String
day01a = show . part1 . parseLines . lines

parseLines :: [String] -> [Int]
parseLines = map $ \case
    'L':n -> negate $ read n
    'R':n -> read n
    _ -> error "impossible"

part1 :: [Int] -> Int
part1 = length . filter ((== 0) . (`mod` 100)) . scanl (+) 50

{------------------------------{ 2nd part }------------------------------}

day01b :: String -> String
day01b = show . part2 . parseLines . lines

part2 :: [Int] -> Int
part2 =  snd . foldl' step (50, 0)
  where
    step (ns, cs) n
      | n >= 100               = step (ns, cs + 1) (n - 100)
      | n <= -100              = step (ns, cs + 1) (n + 100)
      | ns + n >= 100          = ((ns + n) `mod` 100, cs + 1)
      | ns /= 0 && ns + n <= 0 = ((ns + n) `mod` 100, cs + 1)
      | otherwise              = ((ns + n) `mod` 100, cs)
