{-# OPTIONS_GHC -Wno-x-partial #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Days.Day19 (day19a, day19b) where

import qualified Data.Sequence as SQ

{------------------------------{ 1st part }------------------------------}

day19a :: String -> String
day19a = show . part1 . parseLines . lines

parseLines :: [String] -> Int
parseLines = read . head

part1 :: Int -> Int
part1 n = head . head . dropWhile ((> 1) . length) $ iterate stealPresent [1 .. n]

stealPresent :: [Int] -> [Int]
stealPresent xs
  | odd $ length xs = last xs : go xs
  | otherwise = go xs
 where
  go (x : _ : ys) = x : go ys
  go _ = []

{------------------------------{ 2nd part }------------------------------}

day19b :: String -> String
day19b = show . part2 . parseLines . lines

part2 :: Int -> Int
part2 n = elf
 where
  (elf SQ.:< _) = SQ.viewl . head . dropWhile ((> 1) . SQ.length) $ iterate stealPresent' $ SQ.fromList [1 .. n]

stealPresent' :: SQ.Seq Int -> SQ.Seq Int
stealPresent' xs = go (SQ.viewl xs) (SQ.length xs - 2)
 where
  go (elf SQ.:< elfs) nbElfs = SQ.deleteAt (nbElfs `div` 2) elfs SQ.|> elf
