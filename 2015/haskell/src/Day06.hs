{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
module Day06 (day06a, day06b) where

import Data.List.Split (splitOneOf)

data Light = Light Position State  deriving (Eq, Show)
data Position = Position Int Int deriving (Eq, Show)
newtype State = State Bool deriving (Eq, Show)

day06a :: String -> String
day06a = show . countLights . run grid . lines

grid :: [Light]
grid = [Light (Position x y) $ State False | x <- [0..999], y <- [0..999]]

run :: [Light] -> [String] -> [Light]
run x []     = x
run x (('t':'o':'g':'g':'l':'e':' ':ys):zs)         = run (toggle x $ parseLine ys) zs
run x (('t':'u':'r':'n':' ':'o':'f':'f':' ':ys):zs) = run (turnOff x $ parseLine ys) zs
run x (('t':'u':'r':'n':' ':'o':'n':' ':ys):zs)     = run (turnOn x $ parseLine ys) zs
run _ _ = []

parseLine :: String -> (Position, Position)
parseLine line = (Position x1 y1, Position x2 y2)
    where
        [x1,y1,_,x2,y2] = map read (splitOneOf ", " line)

toggle :: [Light] -> (Position,Position) -> [Light]
toggle [] _ = []
toggle (light@(Light pos (State s)):xs) poss@(Position x1 y1, Position x2 y2)
    | inSquare pos (Position x1 y1) (Position x2 y2) = Light pos (State (not s)) : toggle xs poss
    | otherwise = light : toggle xs poss

turnOff :: [Light] -> (Position,Position) -> [Light]
turnOff [] _ = []
turnOff (light@(Light pos _):xs) poss@(Position x1 y1, Position x2 y2)
    | inSquare pos (Position x1 y1) (Position x2 y2) = Light pos (State False) : turnOff xs poss
    | otherwise = light : turnOff xs poss

turnOn :: [Light] -> (Position,Position) -> [Light]
turnOn [] _ = []
turnOn (light@(Light pos _):xs) poss@(Position x1 y1, Position x2 y2)
    | inSquare pos (Position x1 y1) (Position x2 y2) = Light pos (State True) : turnOn xs poss
    | otherwise = light : turnOn xs poss

inSquare :: Position -> Position -> Position -> Bool
inSquare (Position x y) (Position x1 y1) (Position x2 y2) = x >= x1 && y >= y1 && x <= x2 && y <= y2

countLights :: [Light] -> Int
countLights x = length $ filter (\(Light _ (State state)) -> state) x

---------------------------------------------------------------------------
-- Part 2.

data Light' = Light' Position Brightness  deriving (Eq, Show)
newtype Brightness = Brightness Int deriving (Eq, Show)

day06b :: String -> String
day06b = show . countLights' . run' grid' . lines

grid' :: [Light']
grid' = [Light' (Position x y) $ Brightness 0 | x <- [0..999], y <- [0..999]]

run' :: [Light'] -> [String] -> [Light']
run' x []     = x
run' x (('t':'o':'g':'g':'l':'e':' ':ys):zs)         = run' (toggle' x $ parseLine ys) zs
run' x (('t':'u':'r':'n':' ':'o':'f':'f':' ':ys):zs) = run' (turnOff' x $ parseLine ys) zs
run' x (('t':'u':'r':'n':' ':'o':'n':' ':ys):zs)     = run' (turnOn' x $ parseLine ys) zs
run' _ _ = []

toggle' :: [Light'] -> (Position,Position) -> [Light']
toggle' [] _ = []
toggle' (light@(Light' pos (Brightness b)):xs) poss@(Position x1 y1, Position x2 y2)
    | inSquare pos (Position x1 y1) (Position x2 y2) = Light' pos (Brightness (b + 2)) : toggle' xs poss
    | otherwise = light : toggle' xs poss

turnOff' :: [Light'] -> (Position,Position) -> [Light']
turnOff' [] _ = []
turnOff' (light@(Light' pos (Brightness b)):xs) poss@(Position x1 y1, Position x2 y2)
    | inSquare pos (Position x1 y1) (Position x2 y2) = Light' pos (Brightness (if b == 0 then 0 else b - 1)) : turnOff' xs poss
    | otherwise = light : turnOff' xs poss

turnOn' :: [Light'] -> (Position,Position) -> [Light']
turnOn' [] _ = []
turnOn' (light@(Light' pos (Brightness b)):xs) poss@(Position x1 y1, Position x2 y2)
    | inSquare pos (Position x1 y1) (Position x2 y2) = Light' pos (Brightness (b + 1)) : turnOn' xs poss
    | otherwise = light : turnOn' xs poss

countLights' :: [Light'] -> Int
countLights' = foldr (\(Light' _ (Brightness b)) acc -> acc + b) 0
