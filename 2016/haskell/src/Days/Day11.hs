{-# OPTIONS_GHC -Wno-orphans #-}

module Days.Day11 (day11a, day11b) where

import Lib.Algorithm (bfsDist)
import qualified Data.Vector as V
import Data.List (sort)
import Data.Hashable (Hashable, hashWithSalt)

instance Hashable a => Hashable (V.Vector a) where
  hashWithSalt = foldl hashWithSalt

{------------------------------{ 1st part }------------------------------}

-- [Elevator, Chip1, Gen1, Chip2, Gen2, Chip3, Gen3, ...]
type Facility = V.Vector Int

day11a :: String -> String
day11a = show . part1 . parseLines . lines

-- INFO: Manual parsing
parseLines :: [String] -> Facility
-- parseLines _ = V.fromList [0, 0, 1, 0, 2] -- Example
parseLines _ = V.fromList [0, 0, 0, 1, 0, 1, 0, 2, 2, 2, 2]

part1 :: Facility -> Int
part1 fa = bfsDist fa getNeighbours isGoal

getNeighbours :: Facility -> [Facility]
getNeighbours fa = map canonical . filter (isValidFacility n) $ candidates
  where
    e             = elevator fa
    n             = (V.length fa - 1) `div` 2
    possibleFloor = [f | f <- [e-1, e+1], f >= 0, f < 4]

    -- INFO: All movable items on current floor (True for chip, False for gen)
    movable :: [(Int, Bool)]
    movable = [ (i, True)  | i <- [1..n], chipFloor i fa == e ] ++
              [ (i, False) | i <- [1..n], genFloor  i fa == e ]

    -- INFO: 1 or 2 items legal or not
    single = map (:[]) movable

    -- INFO: a < b prevents duplicates (/= would allow some duplicates not <)
    doubles = [ [a,b] | a <- movable, b <- movable, a < b ]
    allMoves = single ++ doubles

    candidates =
      [ foldl (applyMove f) (setElevator f fa) moves
      | f     <- possibleFloor
      , moves <- allMoves
      ]

    applyMove newE fa' (i, True)  = moveChip i newE fa'
    applyMove newE fa' (i, False) = moveGen  i newE fa'

-- INFO: Putting pairs in canonical optimises hashing (represent different facility the same way)
canonical :: Facility -> Facility
canonical v = (v V.! 0) `V.cons` sortedPairs
  where
    n = (V.length v - 1) `div` 2
    pairs = [ (chipFloor i v, genFloor i v) | i <- [1..n] ]
    sortedPairs = V.concat [ V.fromList [c,g] | (c,g) <- sort pairs ]

isValidFacility :: Int -> Facility -> Bool
isValidFacility n fa = all (floorIsSafe fa n) [0..3]

floorIsSafe :: Facility -> Int -> Int -> Bool
floorIsSafe fa n f
  | null chips = True
  | null gens = True
  | otherwise = null friedChips
  where
    chips = [i | i <- [1..n], chipFloor i fa == f]
    gens = [i | i <- [1..n], genFloor i fa == f]
    friedChips = [i | i <- chips, genFloor i fa /= f]

-- INFO: Current elevator position
elevator :: Facility -> Int
elevator fa = fa V.! 0

-- Chip i position in facility
chipFloor :: Int -> Facility -> Int
chipFloor i fa = fa V.! (2 * i - 1)

-- INFO: Gen i position in facility
genFloor :: Int -> Facility -> Int
genFloor i fa = fa V.! (2 * i)

-- INFO: Update elevator value
setElevator :: Int -> Facility -> Facility
setElevator f fa = fa V.// [(0, f)]

-- INFO: Update Chip value
moveChip :: Int -> Int -> Facility -> Facility
moveChip i newFloor fa = fa V.// [(2 * i - 1, newFloor)]

-- INFO: Update Gen value
moveGen :: Int -> Int -> Facility -> Facility
moveGen i newFloor fa = fa V.// [(2 * i, newFloor)]

isGoal :: Facility -> Bool
isGoal = V.all (== 3)

{------------------------------{ 2nd part }------------------------------}

day11b :: String -> String
day11b = show . part2 . parseLines' . lines

-- INFO: Manual parsing
parseLines' :: [String] -> Facility
parseLines' _ = V.fromList [0, 0, 0, 1, 0, 1, 0, 2, 2, 2, 2, 0, 0, 0, 0]

part2 :: Facility -> Int
part2 fa = bfsDist fa getNeighbours isGoal
