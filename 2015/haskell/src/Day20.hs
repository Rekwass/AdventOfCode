{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Day20 (day20a, day20b) where

{------------------------------{ 1st part }------------------------------}

type House = Int
type Elf = Int

day20a :: String -> String
day20a = show . part1 . parseLines . lines

parseLines :: [String] -> Int
parseLines = read . head

houses :: [House]
houses = [1..]

-- elves :: [Elf]
-- elves = [[x, x * 2..] | x <- [1..]]

elvesNb :: [Elf]
elvesNb = [1..]

-- part1 :: Int -> Int
part1 p = ((+1) . length) $ takeWhile (< p) $ countHousePresents houses elvesNb
-- part1 p = countHousePresents houses elvesNb !! 100000

countHousePresents :: [House] -> [Elf] -> [Int]
countHousePresents (h:hs) es = nbPresent : countHousePresents hs es
    where
      elves = filter (\x -> h `mod` x == 0) (take h es)
      nbPresent = sum $ map (*10) elves

{------------------------------{ 2nd part }------------------------------}

day20b :: String -> String
day20b = show . parseLines . lines
