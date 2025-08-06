{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Day13 (day13a, day13b) where

import Text.Regex.PCRE.Heavy (scan, re)
import qualified Data.Matrix as M
import Data.Maybe (mapMaybe)

type Machine = (Button, Button, Prize)
type Button = Point
type Prize = Point
type Point = (Int, Int)

{------------------------------{ 1st part }------------------------------}

day13a :: String -> String
day13a = show . part1 . parseLines . lines

parseLines :: [String] -> [Machine]
parseLines [] = []
parseLines ("":xs) = parseLines xs
parseLines (a:b:p:xs) =
    let
      btnRegex = head . map snd . scan [re|Button (A|B): X\+(\d+), Y\+(\d+)|]
      prizeRegex = head . map snd . scan [re|Prize: X=(\d+), Y=(\d+)|]
      [_, aX, aY] = btnRegex (a :: String)
      [_, bX, bY] = btnRegex (b :: String)
      [xP, yP] = prizeRegex (p :: String)
      btnA = (read aX :: Int, read aY :: Int)
      btnB = (read bX :: Int, read bY :: Int)
      prize = (read xP :: Int, read yP :: Int)
    in (btnA, btnB, prize) : parseLines xs

part1 :: [Machine] -> Int
part1 = sum . map compute . filter valid . mapMaybe winPrize

compute :: (Int, Int) -> Int
compute (x, y) = x * 3 + y

valid :: Point -> Bool
valid (x, y) = x < 100 && y < 100

winPrize :: Machine -> Maybe Prize
winPrize ((a, c), (b, d), (e, f)) =
    let
      mA = M.fromList 2 2 [a,b,c,d]
      mX = M.fromList 2 2 [e,b,f,d]
      mY = M.fromList 2 2 [a,e,c,f]
      detA = M.detLaplace mA
      x = M.detLaplace mX `div` detA
      y = M.detLaplace mY `div` detA
    in (if a * x + b * y == e && c * x + d * y == f then Just (x, y) else Nothing)

{------------------------------{ 2nd part }------------------------------}

day13b :: String -> String
day13b = show . part2 . parseLines . lines

part2 :: [Machine] -> Int
part2 = sum . map compute . mapMaybe (winPrize . updatePrize)

updatePrize :: Machine -> Machine
updatePrize (a, b, (x, y)) = (a, b, (x + 10000000000000, y + 10000000000000))
