module Day08 (day08a, day08b) where
import Data.Maybe (catMaybes)
import Data.List (nubBy, tails)

type Coord = (Int, Int)
type Antenna = (Coord, Char)
type AntiNode = (Coord, Char)

-- (+) and (-) for tuple syntatic sugar

(<+>) :: Coord -> Coord -> Coord
(<+>) (y1, x1) (y2, x2) = (y1 + y2, x1 + x2)

(<->) :: Coord -> Coord -> Coord
(<->) (y1, x1) (y2, x2) = (y1 - y2, x1 - x2)

{------------------------------{ 1st part }------------------------------}

day08a :: String -> String
day08a = show . part1 . parseLines . lines

parseLines :: [String] -> (Int, [Antenna])
parseLines ls = (width, antennas) 
    where
      width = length $ head ls
      antennas = concatMap catMaybes $ zipWith locateAntennas ls [0..]

locateAntennas :: String -> Int -> [Maybe Antenna]
locateAntennas l y = zipWith (\c x -> if c /= '.' then Just ((y,x), c) else Nothing) l [0..]

part1 :: (Int, [Antenna]) -> Int
part1 (w, as) = length $ nubBy duped $ distrib as
    where
      duped x y = fst x == fst y
      distrib = concatMap (\a -> concatMap (placeAntiNodes w (head a)) (tail a)) . init . tails

placeAntiNodes :: Int -> Antenna -> Antenna -> [AntiNode]
placeAntiNodes w (p1, c1) (p2, c2)
    | c1 /= c2                     = []
    | inGrid w np1 && inGrid w np2 = [(np1, c1), (np2, c1)]
    | inGrid w np1                 = [(np1, c1)]
    | inGrid w np2                 = [(np2, c1)]
    | otherwise                    = []
    where
      dist = p1 <-> p2
      np1 = p1 <+> dist
      np2 = p2 <-> dist

inGrid :: Int -> Coord -> Bool
inGrid w (y, x) = x >= 0 && y >= 0 && x < w && y < w

{------------------------------{ 2nd part }------------------------------}

day08b :: String -> String
day08b = show . part2 . parseLines . lines

part2 :: (Int, [Antenna]) -> Int
part2 (w, as) = length $ nubBy duped $ distrib as
    where
      duped x y = fst x == fst y
      distrib = concatMap (\a -> concatMap (placeAntiNodes' w (head a)) (tail a)) . init . tails

placeAntiNodes' :: Int -> Antenna -> Antenna -> [AntiNode]
placeAntiNodes' w (p1, c1) (p2, c2)
    | c1 == c2 = takeWhile inGrid' [(np1, c1) | np1 <- tl] ++ takeWhile inGrid' [(np2, c1) | np2 <- br]
    | otherwise = []
    where
      inGrid' (p', _) = inGrid w p'
      tl = iterate ((p1 <-> p2) <+>) p1
      br = iterate ((p2 <-> p1) <+>) p1
