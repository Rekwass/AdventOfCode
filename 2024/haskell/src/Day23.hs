module Day23 (day23a, day23b) where

import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.List (tails, sort, maximumBy, intercalate)
import Data.Function (on)
import Algorithm (bronKerboschPivot)
import Data.Composition ((.:))

{------------------------------{ 1st part }------------------------------}

type Graph = M.Map String (S.Set String)

day23a :: String -> String
day23a = show . part1 . parseLines

parseLines :: String -> Graph
parseLines = M.unionsWith S.union . map go . lines
  where
    go [a,b,_,d,e] = M.fromList [(l, S.fromList [r]), (r, S.fromList [l])]
      where
        l = [a,b]
        r = [d,e]
    go _ = error "impossible"

part1 :: Graph -> Int
part1 = length . filter doesContainT . groupIfMatch

groupIfMatch :: Graph -> [(String, String, String)]
groupIfMatch g = filter (isConnected g) $ all3Perms $ M.keys g

all3Perms :: [String] -> [(String, String, String)]
all3Perms xs = [(x,y,z) | (x:r1) <- tails xs, (y:r2) <- tails r1, z <- r2]

isConnected :: Graph -> (String, String, String) -> Bool
isConnected g (x,y,z) = 
  x `S.member` yVal && x `S.member` zVal &&
  y `S.member` xVal && y `S.member` zVal &&
  z `S.member` xVal && z `S.member` yVal
  where
    xVal = g M.! x
    yVal = g M.! y
    zVal = g M.! z

doesContainT :: (String, String, String) -> Bool
doesContainT (a,b,c) = startsWithT a || startsWithT b || startsWithT c
  where 
    startsWithT ('t':_) = True
    startsWithT _       = False

{------------------------------{ 2nd part }------------------------------}

day23b :: String -> String
day23b =  show . part2 . parseLines

part2 :: Graph -> String
part2 g = intercalate "," . sort . S.toList . maximumBy (compare `on` S.size) $ cliques
  where
    cliques = bronKerboschPivot pickpivot (g M.!) (S.fromList $ M.keys g)
    pickpivot = S.elemAt 0 .: S.union

