{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Days.Day09 (day09a, day09b) where

import Linear.V2 (V2 (V2))
import Data.List.Split (splitOn)

{------------------------------{ 1st part }------------------------------}

type Coord = V2 Int

day09a :: String -> String
day09a = show . part1 . parseLines . lines

parseLines :: [String] -> [Coord]
parseLines = map parseLine
  where
    parseLine l = let [sx,sy] = splitOn "," l in V2 (read sy) (read sx)

part1 :: [Coord] -> Int
part1 xs = maximum $ [ computeArea x y | x <- xs, y <- xs, x /= y]

computeArea :: Coord -> Coord -> Int
computeArea (V2 y1 x1) (V2 y2 x2) = (abs (y1 - y2) + 1) * (abs (x1 - x2) + 1)

{------------------------------{ 2nd part }------------------------------}

day09b :: String -> String
day09b = show . part2 . parseLines . lines

part2 :: [Coord] -> Int
part2 xs = maximum $ [ computeArea x y | x <- xs, y <- xs, x /= y, not $ segmentsInside x y xs]

segmentsInside :: Coord -> Coord -> [Coord] -> Bool
segmentsInside (V2 y1 x1) (V2 y2 x2) (x:xs) = not $ all segmentOutside borders
  where
    borders = zip (x:xs) (xs ++ [x])
    segmentOutside (V2 sy1 sx1, V2 sy2 sx2) =
         max sy1 sy2 <= min y1 y2
      || min sy1 sy2 >= max y1 y2
      || max sx1 sx2 <= min x1 x2
      || min sx1 sx2 >= max x1 x2

-- displayMap :: [Coord] -> [Coord] -> String
-- displayMap xs ys = intercalate "\n" [[addChar (V2 y x) | x <- [minX..maxX]] | y <- [minY..maxY]]
--   where
--     addChar p
--       | p `elem` xs = '#'
--       | p `elem` ys = 'X'
--       | otherwise   = '.'
--     minY = (+ (-1)) . minimum . map (\(V2 y _) -> y) $ xs
--     maxY = (+1) . maximum . map (\(V2 y _) -> y) $ xs
--     minX = (+ (-2)) . minimum . map (\(V2 _ x) -> x) $ xs
--     maxX = (+2) . maximum . map (\(V2 _ x) -> x) $ xs
