module Lib.Numbers (diagonalCoordinates, combinations, primes, manhattan) where

-- >>> take 5 diagonalCoordinates
-- [(0,0),(1,0),(0,1),(2,0),(1,1)]
diagonalCoordinates :: [(Int, Int)]
diagonalCoordinates = [(i, j) | n <- [0..], i <- [n, n-1..0], let j = n - i]

-- >>> take 8 $ combinations "abcde" 3
-- ["abc","abd","abe","acd","ace","ade","bcd","bce"]
combinations :: [a] -> Int -> [[a]]
combinations _  0 = [[]]
combinations [] _ = []
combinations (x:xs) n
  | n < 0     = []
  | otherwise = map (x:) (combinations xs (n-1)) ++ combinations xs n

-- >>> take 8 primes
-- [2,3,5,7,11,13,17,19]
primes :: [Int]
primes = 2 : filter isPrime [3,5..]
  where
    isPrime n = all (\p -> n `mod` p /= 0) $ takeWhile (\d -> d*d <= n) primes

-- >>> manhattan (-12, 35)
-- 47
manhattan :: (Int, Int) -> Int
manhattan (y, x) = abs y + abs x
