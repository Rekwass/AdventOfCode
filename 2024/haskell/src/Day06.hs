{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Day06 (day06a, day06b) where

import qualified Data.Set as S

import Data.Maybe (fromJust, catMaybes)
import Data.List (elemIndex, nub, delete)

type Point = (Int, Int)
data Direction = North | East | South | West deriving (Eq, Show, Ord)

{------------------------------{ 1st part }------------------------------}

day06a :: String -> String
day06a = show . part1 . parseLines . lines

parseLines :: [String] -> (Int, Point, S.Set Point)
parseLines ls = (width, guard, obstacles)
    where
      width = length $ head ls
      guard = locateGuard (concat ls) width
      obstacles = S.fromList $ concatMap catMaybes $ zipWith locateObstacles [0..] ls

locateGuard :: String -> Int -> Point
locateGuard l w = (y,x)
    where
      i = fromJust $ elemIndex '^' l
      y = i `div` w
      x = i `mod` w

locateObstacles :: Int -> String -> [Maybe Point]
locateObstacles y = zipWith go [0..]
    where
      go x '#' = Just (y,x)
      go _ _ = Nothing

part1 :: (Int, Point, S.Set Point) -> Int
part1 (w, g, os) = length $ nub $ map fst $ takeWhile (inBounds (0, w) . fst) $ iterate (step os) (g, North)

step :: S.Set Point -> (Point, Direction) -> (Point, Direction)
step os (p, d)
    | nstep `S.member` os = step os (p, rotateRight d)
    | otherwise = (nstep, d)
    where
      nstep = stepInDir p d

inBounds :: (Int, Int) -> Point -> Bool
inBounds (tl, br) (y, x) = y >= tl && x >= tl && y < br && x < br

rotateRight :: Direction -> Direction
rotateRight North = East
rotateRight East = South
rotateRight South = West
rotateRight West = North

stepInDir :: Point -> Direction -> Point
stepInDir (y, x) North = (y - 1, x)
stepInDir (y, x) East  = (y, x + 1)
stepInDir (y, x) South = (y + 1, x)
stepInDir (y, x) West  = (y, x - 1)

{------------------------------{ 2nd part }------------------------------}

day06b :: String -> String
day06b = show . part2 . parseLines . lines

part2 :: (Int, Point, S.Set Point) -> Int
part2 (w, g, os) = length $ filter id $ map (isLoop w S.empty (g, North)) obstacles
    where
      obstacles = map (`S.insert` os) $ delete g $ nub $ map fst guardPath
      guardPath = takeWhile (inBounds (0, w) . fst) $ iterate (step os) (g, North)

isLoop :: Int -> S.Set (Point, Direction) -> (Point, Direction) -> S.Set Point -> Bool
isLoop w trails g@(_, d) os
    | not . inBounds (0, w) $ fst nextPos = False
    | hasMoved && g `S.member` trails = True
    | hasMoved = isLoop w (g `S.insert` trails) nextPos os
    | otherwise = isLoop w trails nextPos os
    where
      nextPos@(_, nd) = step os g
      hasMoved = d /= nd
