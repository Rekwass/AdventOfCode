module Day21 (day21a, day21b) where
import Data.List.Split (splitOn)

{------------------------------{ 1st part }------------------------------}

type HitPoint = Int
type Gold     = Int
type Damage   = Int
type Armour    = Int
type Player   = (HitPoint, Damage, Armour)
type Boss     = (HitPoint, Damage, Armour)

weapons :: [(Gold, Damage, Armour)]
weapons = [(8 , 4, 0),
           (10, 5, 0),
           (25, 6, 0),
           (40, 7, 0),
           (74, 8, 0)]

armours :: [(Gold, Damage, Armour)]
armours = [(13 , 0, 1),
          (31 , 0, 2),
          (53 , 0, 3),
          (75 , 0, 4),
          (102, 0, 5)]

rings :: [(Gold, Damage, Armour)]
rings = [(25 , 1, 0),
         (50 , 2, 0),
         (100, 3, 0),
         (20 , 0, 1),
         (40 , 0, 2),
         (80 , 0, 3)]

player :: Player
player = (100, 0, 0)

day21a :: String -> String
day21a = show . part1 . parseLines . lines

parseLines :: [String] -> Boss
parseLines [shp, sdmg, sarmor] = (read hp, read dmg, read armor)
  where
    hp = (!! 2) . splitOn " " $ shp
    dmg = (!! 1) . splitOn " " $ sdmg
    armor = (!! 1) . splitOn " " $ sarmor

part1 :: Boss -> Int
part1 boss = fst $ foldr1 (\(g, p) (cg, cp) -> if g < cg && fight p boss then (g, p) else (cg, cp)) allPlayerStats

fight :: Player -> Boss -> Bool
fight (ph, pd, pa) (bh, bd, ba)
  | nextBh <= 0 = True
  | nextPh <= 0 = False
  | otherwise = fight (nextPh, pd, pa) (nextBh, bd, ba)
  where
    nextBh = bh - max 1 (pd - ba)
    nextPh = ph - max 1 (bd - pa)

allComb :: [(Gold, Damage, Armour)]
allComb = [foldr addStats (foldr addStats w a) r | w <- weapons, a <- allArmours, r <- allRings]
  where
    addStats (g1, d1, a1) (g2, d2, a2) = (g1 + g2, d1 + d2, a1 + a2)
    allArmours = [] : map (:[]) armours
    allRings = [] : map (:[]) rings ++ [[l, r] | l <- rings, r <- rings, l /= r]

allPlayerStats :: [(Gold, Player)]
allPlayerStats = map (`applyStats` player) allComb
  where
    applyStats (g, d, a) (hp, pd, pa) = (g,(hp, d + pd, a + pa))

{------------------------------{ 2nd part }------------------------------}

day21b :: String -> String
day21b = show . part2 . parseLines . lines

part2 :: (HitPoint, Damage, Armour) -> Int
part2 boss = fst $ foldr (\(g, p) (cg, cp) -> if g > cg && not (fight p boss) then (g, p) else (cg, cp)) (0, player) allPlayerStats
