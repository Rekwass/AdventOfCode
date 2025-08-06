{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Day15 (day15a, day15b) where

import qualified Data.Set as S
import Data.List.Split (splitOn)
import Linear.V2 (V2 (V2))
import Data.List (intercalate, foldl')

type Robot = Point
type Boxes = S.Set Box
type Box = Point
type Walls = S.Set Wall
type Wall = Point
type Point = V2 Int

{------------------------------{ 1st part }------------------------------}

day15a :: String -> String
day15a = show . part1 . parseLines . lines

parseLines :: [String] -> (Robot, Boxes, Walls, [Point])
parseLines ls = (r, bs, ws, ms)
    where
      [mapls, movels] = splitOn [""] ls
      r = head [V2 y x | (y, row) <- zip [0..] mapls, (x, c) <- zip [0..] row, c == '@']
      bs = S.fromList [V2 y x | (y, row) <- zip [0..] mapls, (x, c) <- zip [0..] row, c == 'O']
      ws = S.fromList [V2 y x | (y, row) <- zip [0..] mapls, (x, c) <- zip [0..] row, c == '#']
      ms = map getDir $ concat movels

getDir :: Char -> Point
getDir '^' = V2 (-1) 0
getDir '>' = V2 0 1
getDir 'v' = V2 1 0
getDir '<' = V2 0 (-1)

part1 :: (Robot, Boxes, Walls, [Point]) -> Int
part1 (r, bs, ws, ms) = computeScore $ snd $ step r bs ws ms
-- part1 (r, bs, ws, ms) = displayMap nr nbs ws
--     where
--       (nr, nbs) = moveRobot r bs ws ms

computeScore :: Boxes -> Int
computeScore = S.foldr (\(V2 y x) acc -> 100 * y + x + acc) 0

step :: Robot -> Boxes -> Walls -> [Point] -> (Robot, Boxes)
step r bs ws = foldl' (uncurry move') (r, bs)
    where
      move' nr nbs d = if validMove (nr + d) nbs ws d then moveRobot (nr + d) nbs d else (nr, nbs)     

validMove :: Robot -> Boxes -> Walls -> Point -> Bool
validMove r bs ws d
    | r `S.member` ws = False
    | r `S.member` bs = validMove (r + d) bs ws d
    | otherwise = True

moveRobot :: Robot -> Boxes -> Point -> (Robot, Boxes)
moveRobot r bs d
    | r `S.member` bs = (r, moveBoxes (r + d) (S.delete r bs) d)
    | otherwise = (r, bs)

moveBoxes :: Box -> Boxes -> Point -> Boxes
moveBoxes b bs d
    | b `S.member` bs = moveBoxes (b + d) bs d
    | otherwise = S.insert b bs

displayMap :: Robot -> Boxes -> Walls -> [String]
displayMap r bs ws = [line y | y <- [0..h]]
    where
      V2 h w = S.findMax ws
      line y = [addChar (V2 y x) | x <- [0..w]]
      addChar p
        | p == r = '@'
        | p `S.member` bs = 'O'
        | p `S.member` ws = '#'
        | otherwise = '.'

{------------------------------{ 2nd part }------------------------------}

day15b :: String -> String
day15b = show . part2 . parseLines' . lines

parseLines' :: [String] -> (Robot, Boxes, Walls, [Point])
parseLines' ls = (r, bs, ws, ms)
    where
      [mapls, movels] = splitOn [""] ls
      r = head [V2 y x | (y, row) <- zip [0..] mapls, (x, c) <- zip [0,2..] row, c == '@']
      bs = S.fromList [V2 y x | (y, row) <- zip [0..] mapls, (x, c) <- zip [0,2..] row, c == 'O']
      ws = S.fromList $ concat [[V2 y x, V2 y (x+1)] | (y, row) <- zip [0..] mapls, (x, c) <- zip [0,2..] row, c == '#']
      ms = map getDir $ concat movels

part2 :: (Robot, Boxes, Walls, [Point]) -> Int
part2 (r, bs, ws, ms) = computeScore $ snd $ step' r bs ws ms
-- part2 (r, bs, ws, ms) = displayMap' nr nbs ws
--     where
--       (nr, nbs) = move2 r bs ws ms

step' :: Robot -> Boxes -> Walls -> [Point] -> (Robot, Boxes)
step' r bs ws = foldl' (uncurry move2') (r, bs)
    where
      move2' nr nbs d = if validMove' nr nbs ws d then moveRobot' nr nbs d else (nr, nbs)     

validMove' :: Robot -> Boxes -> Walls -> Point -> Bool
validMove' r bs ws d
    | nr `S.member` ws = False
    | d == td && nr `S.member` bs = validMove' nr bs ws d && validMove' (nr + rd) bs ws d -- Top (left)
    | d == td && (nr + ld) `S.member` bs = validMove' (nr + ld) bs ws d && validMove' nr bs ws d -- Top (right)

    | d == rd && nr `S.member` bs = validMove' (nr + d) bs ws d -- Right

    | d == bd && nr `S.member` bs = validMove' nr bs ws d && validMove' (nr + rd) bs ws d -- Down (left)
    | d == bd && (nr + ld) `S.member` bs = validMove' (nr + ld) bs ws d && validMove' nr bs ws d -- Down (right)

    | d == ld && (nr + d) `S.member` bs = validMove' (nr + d) bs ws d -- Left
    | otherwise = True
    where
      nr = r + d
      td = V2 (-1) 0
      rd = V2 0 1
      bd = V2 1 0
      ld = V2 0 (-1)

moveRobot' :: Robot -> Boxes -> Point -> (Robot, Boxes)
moveRobot' r bs d
    | d == td && nr `S.member` bs = (nr, moveBoxes' nr (S.delete nr bs) d) -- Top (left)
    | d == td && (nr + ld) `S.member` bs = (nr, moveBoxes' (nr + ld) (S.delete (nr + ld) bs) d) -- Top (right)

    | d == rd && nr `S.member` bs = (nr, moveBoxes' nr (S.delete nr bs) d) -- Right

    | d == bd && nr `S.member` bs = (nr, moveBoxes' nr (S.delete nr bs) d) -- Down (left)
    | d == bd && (nr + ld) `S.member` bs = (nr, moveBoxes' (nr + ld) (S.delete (nr + ld) bs) d) -- Down (right)

    | d == ld && (nr + d) `S.member` bs = (nr, moveBoxes' (nr + d) (S.delete (nr + d) bs) d) -- Left
    | otherwise = (nr, bs)
    where
      nr = r + d
      td = V2 (-1) 0
      rd = V2 0 1
      bd = V2 1 0
      ld = V2 0 (-1)

moveBoxes' :: Box -> Boxes -> Point -> Boxes
moveBoxes' b bs d
    | d == td && nb `S.member` bs = S.insert nb $ moveBoxes' nb (S.delete nb bs) d -- Top Top
    | d == td && (nb + ld) `S.member` bs && (nb + rd) `S.member` bs = S.insert nb $ moveBoxes' (nb + rd) (S.delete (nb + rd) left) d -- Top Both
    | d == td && (nb + ld) `S.member` bs = S.insert nb $ moveBoxes' (nb + ld) (S.delete (nb + ld) bs) d -- Top Left
    | d == td && (nb + rd) `S.member` bs = S.insert nb $ moveBoxes' (nb + rd) (S.delete (nb + rd) bs) d -- Top Right

    | d == rd && (nb + d) `S.member` bs = S.insert nb $ moveBoxes' (nb + d) (S.delete (nb + d) bs) d  -- Right

    | d == bd && nb `S.member` bs = S.insert nb $ moveBoxes' nb (S.delete nb bs) d -- Bot Bot
    | d == bd && (nb + ld) `S.member` bs && (nb + rd) `S.member` bs = S.insert nb $ moveBoxes' (nb + rd) (S.delete (nb + rd) left) d -- Bot Both
    | d == bd && (nb + ld) `S.member` bs = S.insert nb $ moveBoxes' (nb + ld) (S.delete (nb + ld) bs) d -- Bot Left
    | d == bd && (nb + rd) `S.member` bs = S.insert nb $ moveBoxes' (nb + rd) (S.delete (nb + rd) bs) d -- Bot Right

    | d == ld && (nb + d) `S.member` bs = S.insert nb $ moveBoxes' (nb + d) (S.delete (nb + d) bs) d  -- Left

    | otherwise = S.insert nb bs
    where
      nb = b + d
      td = V2 (-1) 0
      rd = V2 0 1
      bd = V2 1 0
      ld = V2 0 (-1)
      left = moveBoxes' (nb + ld) (S.delete (nb + ld) bs) d

displayMap' :: Robot -> Boxes -> Walls -> String
displayMap' r bs ws = intercalate "\n" [line y | y <- [0..h]]
    where
      V2 h w = S.findMax ws
      line y = [addChar (V2 y x) | x <- [0..w]]
      addChar p
        | p == r = '@'
        | p `S.member` bs = '['
        | (p + V2 0 (-1)) `S.member` bs = ']'
        | p `S.member` ws = '#'
        | otherwise = '.'
