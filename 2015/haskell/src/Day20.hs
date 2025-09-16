{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Day20 (day20a, day20b) where

import Data.List (group, find)
import Data.Maybe (fromJust)

{------------------------------{ 1st part }------------------------------}

day20a :: String -> String
day20a = show . part1 . parseLines . lines

parseLines :: [String] -> Int
parseLines = read . head

part1 :: Int -> Int
part1 n = fromJust $ find ((\d -> d >= n `div` 10) . goodElfs) [1..]

goodElfs :: Int -> Int
goodElfs = sum . divisors

divisors :: Int -> [Int]
divisors = map product . mapM (scanl (*) 1) . group . factorize

primes :: [Int]
primes = 2 : filter isPrime [3,5..]
  where
    isPrime n = all (\p -> n `mod` p /= 0) $ takeWhile (\d -> d*d <= n) primes

factorize :: Int -> [Int]
factorize 1 = []
factorize n = go n primes
  where
    go m (p:ps)
      | p * p > m = [m | m > 1]
      | r == 0    = p : go q (p:ps)
      | otherwise = go m ps
      where
        (q, r) = m `divMod` p

{------------------------------{ 2nd part }------------------------------}

day20b :: String -> String
day20b = show . part2 . parseLines . lines

part2 :: Int -> Int
part2 n = fromJust $ find ((\d -> d >= n `div` 11) . lazyElfs) [1..]

lazyElfs :: Int -> Int
lazyElfs n = sum . filter (\d -> d * 50 >= n) . divisors $ n
