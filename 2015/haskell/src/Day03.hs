module Day03 (day03a, day03b) where

import Data.List (nub)

data Position = Position Int Int deriving Eq

day03a :: String -> String
day03a x = show . length $ nub $ countHouses x (Position 0 0)

day03b :: String -> String
day03b x = show . length $ nub $ countHouses' x (Position 0 0) $ Position 0 0

countHouses :: String -> Position -> [Position]
countHouses []     _        = []
countHouses (x:xs) position = countHouses xs (newPosition x position) ++ [position]

countHouses' :: String -> Position -> Position -> [Position]
countHouses' []     _        _          = []
countHouses' (x:y:xs) santaPos robotPos = countHouses' xs (newPosition x santaPos) (newPosition y robotPos) ++ [santaPos, robotPos]
countHouses' (x:xs)   santaPos robotPos = countHouses' xs (newPosition x santaPos) robotPos ++ [santaPos]

newPosition :: Char -> Position -> Position
newPosition '^' (Position x y) = Position x       (y + 1)
newPosition '>' (Position x y) = Position (x + 1) y
newPosition 'v' (Position x y) = Position x       (y - 1)
newPosition '<' (Position x y) = Position (x - 1) y
newPosition _   position       = position
