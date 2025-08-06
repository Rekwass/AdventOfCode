{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
module Day14 (day14a, day14b) where

import Text.Regex.PCRE.Heavy (scan, re)
import qualified Data.Set as S

type Robot = (Point, Velocity)
type Point = (Int, Int)
type Velocity = Point

{------------------------------{ 1st part }------------------------------}

day14a :: String -> String
day14a = show . part1 . parseLines . lines

parseLines :: [String] -> [Robot]
parseLines [] = []
parseLines (l:ls) =
    let
      [x, y, vx, vy] = head $ map snd $ scan [re|p=(\d+),(\d+) v=(-?\d+),(-?\d+)|] l
    in ((read x, read y), (read vx, read vy)) : parseLines ls

part1 :: [Robot] -> Int
part1 rs = product $ countRobot $ map (`moveRobot` 100) rs

moveRobot :: Robot -> Int -> Robot
moveRobot ((x, y), (vx, vy)) n = (((x + vx * n) `mod` 101, (y + vy * n) `mod` 103), (vx, vy))

countRobot :: [Robot] -> [Int]
countRobot rs = [tl, tr, br, bl]
    where
      h = 103 `div` 2
      w = 101 `div` 2
      tl = length $ filter (\((x, y), _) -> x < w && y < h) rs
      tr = length $ filter (\((x, y), _) -> x > w && y < h) rs
      br = length $ filter (\((x, y), _) -> x < w && y > h) rs
      bl = length $ filter (\((x, y), _) -> x > w && y > h) rs

{------------------------------{ 2nd part }------------------------------}

day14b :: String -> String
day14b = show . part2 . parseLines . lines

part2 :: [Robot] -> Int
part2 rs = fst $ zip is niceMaps !! 1
    where
      is = map snd $ take 3 $ filter fst $ zip (map robotNoOverlap (iterate (map (`moveRobot` 1)) rs)) [0..]
      mapsWithoutOverLap = map (\i -> map (`moveRobot` i) rs) is
      niceMaps = map (displayMap' 101 103 . S.fromList . map fst) mapsWithoutOverLap

robotNoOverlap :: [Robot] -> Bool
robotNoOverlap rs = length rs == S.size (S.fromList (map fst rs))

displayMap' :: Int -> Int -> S.Set Point -> [String]
displayMap' w h pss = [line y | y <- [0..h]]
    where
      line y = [nbRobots pss x y | x <- [0..w]]

nbRobots :: S.Set Point -> Int -> Int -> Char
nbRobots ps x y = if S.member (x, y) ps then '#' else '.'
