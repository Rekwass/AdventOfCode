module Days.Day17 (day17a, day17b) where

import Lib.Algorithm (bfs, dfsAllDists)
import Linear.V2 (V2 (V2))
import qualified Crypto.Hash.MD5 as MD5
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Base16 as B16
import qualified Data.Map as M
import qualified Data.Sequence as SQ
import qualified Data.HashSet as HS
import Data.Maybe (fromJust, isNothing)

type Point = V2 Int
type Passcode = String

{------------------------------{ 1st part }------------------------------}

day17a :: String -> String
day17a = show . part1 . head . lines

part1 :: String -> String
part1 h = myBfs (V2 0 0, h) getNeighbours isGoal

myBfs :: (Point, Passcode) -> ((Point, Passcode) -> [(Point, Passcode)]) -> ((Point, Passcode) -> Bool) -> Passcode
myBfs start getNeighbours' isGoal'
  | isNothing mPath = ""
  | otherwise = dropWhile (not . (`elem` "UPLR")) $ snd end
    where
      mPath = bfs queue explored prev getNeighbours' isGoal'
      path = fromJust mPath
      end = head . filter isGoal . M.keys $ path
      queue = SQ.singleton start
      explored = HS.empty
      prev = M.empty

getNeighbours :: (Point, Passcode) -> [(Point, Passcode)]
getNeighbours (V2 y x, pcode) = ps
  where
    ps = [ (V2 (y + dy) (x + dx), pcode ++ [dir])
         | ((dy, dx, dir), c) <- zip dirs hash
         , inBound (y + dy) (x + dx)
         , isOpen c]
    hash = take 4 . B8.unpack . B16.encode . MD5.hash . B8.pack $ pcode

inBound :: Int -> Int -> Bool
inBound y x = y >= 0 && y <= 3 && x >= 0 && x <= 3

isOpen :: Char -> Bool
isOpen = flip elem "bcdef"

-- INFO: Directions reversed for y coordinate, we start top left (0,0) end bottom right (3,3)
dirs :: [(Int, Int, Char)]
dirs = [(-1, 0, 'U'), (1, 0, 'D'), (0, -1, 'L'), (0, 1, 'R')]

isGoal :: (Point, Passcode) -> Bool
isGoal (V2 3 3, _) = True
isGoal _           = False

{------------------------------{ 2nd part }------------------------------}

day17b :: String -> String
day17b = show . part2 . head . lines

part2 :: String -> Int
part2 h = maximum $ dfsAllDists (V2 0 0, h) getNeighbours isGoal
