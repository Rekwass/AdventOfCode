{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Day14 (day14a, day14b) where

import Data.List.Split (splitOn)

data Reindeer = Reindeer String Int Int Int deriving (Eq, Show)

{-
INFO: A better way to solve this exercise would have been to avoid the use of `iterateWithIndex` and find an alternative way to add up reindeers progression.
I could have used `cycle` and `replicate` to create the `race` of each indivual reindeer.
This would be a more `haskellic` way do solve it. Avoid using indexes when possible.
-}

day14a :: String -> String
day14a = show . run . parseLines . map (splitOn " ") . lines

parseLines :: [[String]] -> [Reindeer]
parseLines [] = []
parseLines ([n,_,_,s,_,_,d,_,_,_,_,_,_,r,_]:xs) = Reindeer n (read s) (read d) (read r):parseLines xs

run :: [Reindeer] -> Int
-- INFO:                                                          or (zip3 rs (0 <$ rs) (0 <$ rs))
run rs = maximum $ map (\(_,d,_) -> d) $ (iterateWithIndex moveReindeers [(r,0,0) | r <- rs]) !! 2503

iterateWithIndex :: (a -> Int -> a) -> a -> [a]
iterateWithIndex f x = x : zipWith f (iterateWithIndex f x) [0..]

moveReindeers :: [(Reindeer, Int, Int)] -> Int -> [(Reindeer, Int, Int)]
moveReindeers rs time = map (moveReindeer time) rs

moveReindeer :: Int -> (Reindeer, Int, Int) -> (Reindeer, Int, Int)
moveReindeer time og@(re@(Reindeer _ s _ _), dst, score) = if resting re time then og else (re, dst + s, score)

resting :: Reindeer -> Int -> Bool
resting (Reindeer _ _ d rt) time = (time `mod` (d + rt)) >= d

---------------------------------------------------------------------------
-- Part 2.

day14b :: String -> String
day14b = show . run' . parseLines . map (splitOn " ") . lines

run' :: [Reindeer] -> Int
run' rs = maximum $ map (\(_,_,s) -> s) $ (iterateWithIndex moveReindeers' [(r,0, 0) | r <- rs]) !! 2503

moveReindeers' :: [(Reindeer, Int, Int)] -> Int -> [(Reindeer, Int, Int)]
moveReindeers' rs count = addOneToBest $ map (moveReindeer count) rs

addOneToBest :: [(Reindeer, Int, Int)] -> [(Reindeer, Int, Int)]
addOneToBest rs = map (\re@(r, dst, s) -> if dst == getMaximumReindeerDst' rs then (r, dst, s + 1) else re) rs

getMaximumReindeerDst' :: [(Reindeer, Int, Int)] -> Int
getMaximumReindeerDst' = foldr (max . (\(_, d, _) -> d)) 0
