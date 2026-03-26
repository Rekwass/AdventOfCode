{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-x-partial #-}

module Days.Day15 (day15a, day15b) where

import qualified Data.Text as T
import Text.Parsec
import Text.Parsec.Text (Parser)
import Data.Either (fromRight)

{------------------------------{ 1st part }------------------------------}

type Disc = (Int, Int)

day15a :: String -> String
day15a = show . part1 . parseLines . T.lines . T.pack

parseLines :: [T.Text] -> [Disc]
parseLines = map (fromRight (error "impossible") . parse lineParser "")

lineParser :: Parser Disc
lineParser = (,)
  <$> (string "Disc #" >> number >> string " has " *> number)
  <*> (string " positions; at time=" >> number >> string ", it is at position " *> number)

number :: Parser Int
number = read <$> many1 digit

part1 :: [Disc] -> Int
part1 xs = head . filter (isWinning xs) $ [0..]

isWinning :: [Disc] -> Int -> Bool
isWinning xs t = all (`aligned` t) $ zip xs [1..]

aligned :: (Disc, Int) -> Int -> Bool
aligned ((nbPos, offset), level) t = ((offset + level + t) `mod` nbPos) == 0

{------------------------------{ 2nd part }------------------------------}

day15b :: String -> String
day15b = show . part2 . parseLines . T.lines . T.pack

part2 :: [Disc] -> Int
part2 xs = head . filter (isWinning (xs ++ [(11,0)])) $ [0..]
