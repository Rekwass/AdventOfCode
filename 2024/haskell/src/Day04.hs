{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use head" #-}
module Day04 (day04a, day04b) where

import Data.List (zipWith7, transpose)

type Grid = [Row]
type Row = String

{------------------------------{ 1st part }------------------------------}

day04a :: String -> String
day04a = show . part1 . lines

part1 :: Grid -> Int
part1 g = sum $ map sum $ zipWith7 moveSquareDown gz (drop 1 gz) (drop 2 gz) (drop 3 gz) (drop 4 gz) (drop 5 gz) (drop 6 gz)
    where
      gz = zeroLines ++ padRow g ++ zeroLines
      zeroLines = [zeroLine] ++ [zeroLine] ++ [zeroLine]
      zeroLine = replicate (length (head g) + 6) '0'
      padRow = map (\r -> "000" ++ r ++ "000")

moveSquareDown :: Row -> Row -> Row -> Row -> Row -> Row -> Row -> [Int]
moveSquareDown as bs cs ds es fs gs = zipWith countXmas (drop 3 ds) $ takeWhile ((==48) . length) $ transpose
         [as, drop 1 as, drop 2 as, drop 3 as, drop 4 as, drop 5 as, drop 6 as,
          bs, drop 1 bs, drop 2 bs, drop 3 bs, drop 4 bs, drop 5 bs, drop 6 bs,
          cs, drop 1 cs, drop 2 cs, drop 3 cs, drop 4 cs, drop 5 cs, drop 6 cs,
          ds, drop 1 ds, drop 2 ds,            drop 4 ds, drop 5 ds, drop 6 ds,
          es, drop 1 es, drop 2 es, drop 3 es, drop 4 es, drop 5 es, drop 6 es,
          fs, drop 1 fs, drop 2 fs, drop 3 fs, drop 4 fs, drop 5 fs, drop 6 fs,
          gs, drop 1 gs, drop 2 gs, drop 3 gs, drop 4 gs, drop 5 gs, drop 6 gs]

countXmas :: Char -> String -> Int
countXmas c cs = t + tr + r + br + b + bl + l + tl
    where
      t  = isXmas [c, cs !! 17, cs !! 10, cs !! 3 ]
      tr = isXmas [c, cs !! 18, cs !! 12, cs !! 6 ]
      r  = isXmas [c, cs !! 24, cs !! 25, cs !! 26]
      br = isXmas [c, cs !! 31, cs !! 39, cs !! 47]
      b  = isXmas [c, cs !! 30, cs !! 37, cs !! 44]
      bl = isXmas [c, cs !! 29, cs !! 35, cs !! 41]
      l  = isXmas [c, cs !! 23, cs !! 22, cs !! 21]
      tl = isXmas [c, cs !! 16, cs !! 8,  cs !! 0 ]

isXmas :: String -> Int
isXmas "XMAS" = 1
isXmas _ = 0

{------------------------------{ 2nd part }------------------------------}

day04b :: String -> String
day04b = show . part2 . lines

part2 :: Grid -> Int
part2 g = sum $ map sum $ zipWith3 moveSquareDown' gz (drop 1 gz) (drop 2 gz)
    where
      gz = [zeroLine] ++ padRow g ++ [zeroLine]
      zeroLine = replicate (length (head g) + 2) '0'
      padRow = map (\r -> "0" ++ r ++ "0")

moveSquareDown' :: Row -> Row -> Row -> [Int]
moveSquareDown' as bs cs = zipWith countXmas' (drop 1 bs) $ takeWhile ((==8) . length) $ transpose
         [as, drop 1 as, drop 2 as,
          bs,            drop 2 bs,
          cs, drop 1 cs, drop 2 cs]

countXmas' :: Char -> String -> Int
countXmas' c cs = t + r + b + l
    where
      t  = isXmas' [cs !! 0, c, cs !! 7, cs !! 2, cs !! 5]
      r  = isXmas' [cs !! 2, c, cs !! 5, cs !! 7, cs !! 0]
      b  = isXmas' [cs !! 7, c, cs !! 0, cs !! 5, cs !! 2]
      l  = isXmas' [cs !! 5, c, cs !! 2, cs !! 0, cs !! 7]

isXmas' :: String -> Int
isXmas' "MASMS" = 1
isXmas' _ = 0

{------------------------------{ Explanation }------------------------------}

{-
When I have index base search in a 2D array, I use a `moving square technique`.
For part 1 I need to zero pad the array with a width of 3 because you can search for the word `XMAS` in all 8 directions

Then I have my square that will move automatically with `zipWith7` and `zipWith`

When 0 padded it looks like this

0000000000000000
0000000000000000
0000000000000000
000MMMSXXMASM000
000MSAMXMSMSA000
000AMXSXMAAMM000
000MSAMASMSMX000
000XMASAMXAMM000
000XXAMMXXAMA000
000SMSMSASXSS000
000SAXAMASAAA000
000MAMMMXMMMM000
000MXMXAXMASX000
0000000000000000
0000000000000000
0000000000000000

Then I separate it by lines using zipWith7

+------------------+
| 0000000000000000 |
| 0000000000000000 |
| 0000000000000000 |
| 000MMMSXXMASM000 |
| 000MSAMXMSMSA000 |
| 000AMXSXMAAMM000 |
| 000MSAMASMSMX000 |
+------------------+
  000XMASAMXAMM000
  000XXAMMXXAMA000
  000SMSMSASXSS000
  000SAXAMASAAA000
  000MAMMMXMMMM000
  000MXMXAXMASX000
  0000000000000000
  0000000000000000
  0000000000000000

Finally another zipWith and the use of transpose to create my square and move it automatically to the right

+---------+
| 0000000 | 000000000 
| 0000000 | 000000000 
| 0000000 | 000000000 
| 000MMMS | XXMASM000 
| 000MSAM | XMSMSA000 
| 000AMXS | XMAAMM000 
| 000MSAM | ASMSMX000 
+---------+
  000XMASAMXAMM000
  000XXAMMXXAMA000
  000SMSMSASXSS000
  000SAXAMASAAA000
  000MAMMMXMMMM000
  000MXMXAXMASX000
  0000000000000000
  0000000000000000
  0000000000000000

Now with this, I can easily check each time all the possilities. Using zipWith I will transform each square into an Integer representing the number of `XMAS` that were found.

For part 2 it is the same thing, only that I 0 pad with a width of 1 and that's it

-}
