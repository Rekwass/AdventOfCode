{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}

module Days.Day08 (day08a, day08b) where

import Text.Regex.PCRE.Heavy (re, scan)
import Data.List (foldl', intercalate, transpose)

{------------------------------{ 1st part }------------------------------}

data Instruction = Rect Width Height | Row Height Width | Col Width Height deriving Show

type Height = Int
type Width = Int

type Pixel = Bool
type Screen = [[Pixel]]

day08a :: String -> String
day08a = show . part1 . parseLines . lines

parseLines :: [String] -> [Instruction]
parseLines = map go
  where
    go ('r':'e':'c':'t':_:xs) = 
      let [(_, [x, y])] = scan [re|(\d+)x(\d+)|] xs
      in Rect (read x) (read y)
    go ('r':'o':'t':'a':'t':'e':_:'r':'o':'w':_:xs) =
      let [(_, [y, x])] = scan [re|y=(\d+) by (\d+)|] xs
      in Row (read y) (read x)
    go ('r':'o':'t':'a':'t':'e':_:'c':'o':'l':'u':'m':'n':_:xs) =
      let [(_, [x, y])] = scan [re|x=(\d+) by (\d+)|] xs
      in Col (read x) (read y)

screen :: Screen
screen = [[False | _ <- [0..49]] | _ <- [0..5]]
-- screen = [[False | _ <- [0..6]] | _ <- [0..2]] -- Example

part1 :: [Instruction] -> Int
part1 = countPixels . foldl' step screen

countPixels :: Screen -> Int
countPixels = sum . map (length . filter id)

step :: Screen -> Instruction -> Screen
step s (Rect w h) = createScreen s w h
step s (Row  h w) = rotateRow    s h w
step s (Col  w h) = rotateColumn s w h

createScreen :: Screen -> Width -> Height -> Screen
createScreen s w h = map (turnColumnOn w) l ++ r
  where
    (l, r) = splitAt h s

turnColumnOn :: Width -> [Pixel] -> [Pixel]
turnColumnOn w ps = map turnOn l ++ r
  where
    (l, r) = splitAt w ps

turnOn :: Pixel -> Pixel
turnOn _ = True

rotateRow :: Screen -> Height -> Width -> Screen
rotateRow s h w = l ++ [shiftRight w m] ++ r'
  where
    (l, r) = splitAt h s
    m : r' = r

shiftRight :: Int -> [Pixel] -> [Pixel]
shiftRight shift row = take rlen $ drop (rlen - shift) $ cycle row
  where
    rlen = length row

rotateColumn :: Screen -> Width -> Height -> Screen
rotateColumn s w h = transpose $ rotateRow s' w h
  where
    s' = transpose s

{------------------------------{ 2nd part }------------------------------}

day08b :: String -> String
day08b = part2 . parseLines . lines

part2 :: [Instruction] -> String
part2 = drawScreen . foldl' step screen

drawScreen :: Screen -> String
drawScreen = intercalate "\n" . map (map (\x -> if x then '#' else '.'))
