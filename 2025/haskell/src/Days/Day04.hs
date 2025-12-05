{-# OPTIONS_GHC -Wno-incomplete-uni-patterns -Wno-x-partial #-}

module Days.Day04 (day04a, day04b) where

import qualified Data.Set as S

{------------------------------{ 1st part }------------------------------}

type Coord = (Int, Int)
type Papers = S.Set Coord

day04a :: String -> String
day04a = show . part1 . parseLines . lines

parseLines :: [String] -> Papers
parseLines = S.fromList . concat . zipWith (\y row -> [(y, x) | (x,'@') <- zip [0..] row]) [0..]

part1 :: Papers -> Int
part1 xs = S.size . S.filter (isAccessible xs) $ xs

dirs :: [Coord]
dirs = [(1,0), (1,1), (0,1), (-1,1), (-1,0), (-1,-1), (0,-1), (1,-1)]

isAccessible :: Papers -> Coord -> Bool
isAccessible xs x = (<4) . length . filter (`S.member` xs) $ neighbours x

neighbours :: Coord -> [Coord]
neighbours (y, x) = [(y+dy, x+dx) | (dy, dx) <- dirs]

{------------------------------{ 2nd part }------------------------------}

day04b :: String -> String
day04b = show . part2 . parseLines . lines

part2 :: S.Set Coord -> Int
part2 xs = S.size xs - S.size xs'
  where
    xs' = fst . head . dropWhile canRemovePaper $ zip steps' (tail steps')
    steps' = iterate step xs
    canRemovePaper = uncurry (/=)

step :: Papers -> Papers
step xs = S.filter (not . isAccessible xs) xs
