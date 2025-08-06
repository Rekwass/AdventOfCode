{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Day18 (day18a, day18b) where

import Data.List (transpose)

{------------------------------{ 1st part }------------------------------}

type Grid = [Row]
type Row = [Int]
type Light = Int

day18a :: String -> String
day18a = show . part1 . parseLines . lines

parseLines :: [String] -> Grid
parseLines =  map readLine
    where 
      readLine = map readChar
      readChar '#' = 1
      readChar '.' = 0

part1 :: Grid -> Int
part1 g = countLights $ iterate update g !! 100

countLights :: Grid -> Int
countLights = sum . map sum

update :: Grid -> Grid
update g = zipWith3 updateWithNeighours zg (tail zg) (tail (tail zg))
    where
      zg = zeroPad g

updateWithNeighours :: Row -> Row -> Row -> Row
updateWithNeighours xs ys zs = zipWith toggle (tail ys) $ map sum . takeWhile ((==8) . length) $ transpose
        [xs, tail xs, tail (tail xs),
         ys, {-- --}  tail (tail ys),
         zs, tail zs, tail (tail zs)]

toggle :: Light -> Int -> Light
toggle 1 n = if n == 2 || n == 3 then 1 else 0
toggle 0 n = if n == 3 then 1 else 0

zeroPad :: Grid -> Grid
zeroPad g = [zeroes] ++ map padRow g ++ [zeroes]
    where
      zeroes = replicate width 0
      width = length (head g) + 2
      padRow r = [0] ++ r ++ [0]

{------------------------------{ 2nd part }------------------------------}

day18b :: String -> String
day18b = show . part2 . parseLines . lines

part2 :: Grid -> Int
part2 g = countLights $ iterate update' (hotCorners g) !! 100

update' :: Grid -> Grid
update' = hotCorners . update

hotCorners :: Grid -> Grid
hotCorners g = [addCorner (head g)] ++ (tail . init $ g) ++ [addCorner (last g)]
    where addCorner r = [1] ++ (tail . init $ r) ++ [1]
