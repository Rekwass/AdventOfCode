{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Day22 (day22a, day22b) where

import Prelude

import GHC.Generics (Generic)
import Data.Hashable (Hashable)
import qualified Data.HashPSQ as H
import qualified Data.Map as M
import Data.List.Split (splitOn)
import Algorithm (dijkstra)
import Data.Maybe (isNothing, fromJust)
import Data.Bifunctor (first)

{------------------------------{ 1st part }------------------------------}

type HitPoint = Int
type Mana     = Int
type Damage   = Int
type Armour   = Int
type Turn     = Int

data Playing = PlayerTurn | BossTurn deriving (Eq, Ord, Show, Generic)

data GameState = GameState
  { player :: Player
  , boss :: Boss
  , effects :: [(Turn, Spell)]
  , manaSpent :: Mana -- Total mana spent
  , turn :: Playing
  } deriving (Show, Eq, Ord, Generic)

data Player   = Player
  { hp     :: HitPoint
  , mana   :: Mana
  , armour :: Armour
  } deriving (Generic, Ord, Eq, Show)

data Boss     = Boss
  { bhp     :: HitPoint
  , damage :: Damage
  } deriving (Generic, Ord, Eq, Show)

data Spell = MagicMissile Mana Damage 
           | Drain Mana Damage HitPoint 
           | Shield Mana Turn Armour 
           | Poison Mana Turn Damage 
           | Recharge Mana Turn Mana 
           deriving (Eq, Ord, Show, Generic)

instance Hashable GameState
instance Hashable Player
instance Hashable Boss
instance Hashable Playing
instance Hashable Spell

spells :: [Spell]
spells = [MagicMissile 53 4, Drain 73 2 2, Shield 113 6 7, Poison 173 6 3, Recharge 229 5 101]

ogPlayer :: Player
ogPlayer = Player 50 500 0
-- ogPlayer = Player 10 250 0

day22a :: String -> String
day22a = show . part1 . parseLines . lines

parseLines :: [String] -> Boss
-- parseLines [shp, sdmg] = Boss 14 8 -- 641
-- parseLines [shp, sdmg] = Boss 13 8 -- 226
parseLines [shp, sdmg] = Boss (read bhp') (read dmg')
  where
    bhp' = (!! 2) . splitOn " " $ shp
    dmg' = (!! 1) . splitOn " " $ sdmg
parseLines _ = error "Invalid file"

part1 :: Boss -> Int
part1 b = myDijkstra (GameState ogPlayer b [] 0 PlayerTurn) getNeighbours' isGoal'

-- Give all possible GameStates from current (none if lost)
getNeighbours' :: GameState -> [(GameState, Mana)]
getNeighbours' g
  | g'.player.hp > 0 && g'.boss.bhp <= 0 = [(g', 0)]
  | g'.player.hp <= 0 || g'.player.mana < 53 = [] -- Check if player has enough mana after apply effect
  | g'.turn == BossTurn = map (first removeShield) (bossAttack g')
  | otherwise = map (first removeShield) (chooseSpellAndAttack g')
  where
    g' = applyEffects g

applyEffects :: GameState -> GameState
applyEffects (GameState p b ss m t) = GameState np nb nss m t
  where
    -- (GameState p' b' ss' _ _) = removeShield g
    (np, nb, nss) = foldr applyEffect (p, b, []) ss

removeShield :: GameState -> GameState
removeShield g 
  | any isShield g.effects = g
  | otherwise              = g { player = np }
    where
      np = g.player { armour = 0 }

isShield :: (Turn, Spell) -> Bool
isShield (_, Shield {}) = True
isShield _ = False

applyEffect :: (Turn, Spell) -> (Player, Boss, [(Turn, Spell)]) -> (Player, Boss, [(Turn, Spell)])
applyEffect (tl, s@(Shield _ _ arm)) (p, b, nss)
  | tl == 1   = (p { armour = arm } , b, nss)
  | otherwise = (p { armour = arm}, b, (tl - 1, s):nss)
applyEffect (tl, s@(Poison _ _ dmg)) (p, b, nss)
  | tl == 1   = (p, b { bhp = b.bhp - dmg}, nss)
  | otherwise = (p, b { bhp = b.bhp - dmg}, (tl - 1, s):nss)
applyEffect (tl, s@(Recharge _ _ m)) (p, b, nss)
  | tl == 1   = (p { mana = p.mana + m }, b, nss)
  | otherwise = (p { mana = p.mana + m }, b, (tl - 1, s):nss)
applyEffect _ _ = error "Could not apply effect ?"

chooseSpellAndAttack :: GameState -> [(GameState, Mana)]
chooseSpellAndAttack g = map (castSpell g) availableSpells
  where
    availableSpells = filter (\s -> hasMana s && not (isActive s)) spells
    isActive s = any ((0 ==) . fst) g.effects || s `elem` map snd g.effects
    hasMana (MagicMissile m _) = g.player.mana >= m
    hasMana (Drain m _ _) = g.player.mana >= m
    hasMana (Shield m _ _) = g.player.mana >= m
    hasMana (Poison m _ _) = g.player.mana >= m
    hasMana (Recharge m _ _) = g.player.mana >= m

castSpell :: GameState -> Spell -> (GameState, Mana)
castSpell g (MagicMissile m dmg)     = (g { player = np, boss = nb, manaSpent = g.manaSpent + m, turn = BossTurn}, m)
  where
    np = g.player { mana = g.player.mana - m }
    nb = g.boss { bhp = g.boss.bhp - dmg}
castSpell g (Drain        m dmg dhp) = (g { player = np, boss = nb, manaSpent = g.manaSpent + m, turn = BossTurn }, m)
  where
    np = g.player { hp = g.player.hp + dhp, mana = g.player.mana - m }
    nb = g.boss { bhp = g.boss.bhp - dmg}
castSpell g s@(Shield       m t _)   = (g { player = np, manaSpent = g.manaSpent + m, effects = (t,s):g.effects, turn = BossTurn }, m)
  where
    np = g.player { mana = g.player.mana - m }
castSpell g s@(Poison       m t _)   = (g { player = np, manaSpent = g.manaSpent + m, effects = (t,s):g.effects, turn = BossTurn }, m)
  where
    np = g.player { mana = g.player.mana - m }
castSpell g s@(Recharge     m t _)   = (g { player = np,manaSpent = g.manaSpent + m, effects = (t,s):g.effects, turn = BossTurn }, m)
  where
    np = g.player { mana = g.player.mana - m }

bossAttack :: GameState -> [(GameState, Mana)]
bossAttack g = [(g { player = p, turn = PlayerTurn }, 0)]
  where
    p = g.player { hp = g.player.hp - max (g.boss.damage - g.player.armour) 1 }

-- True if boss dead
isGoal' :: GameState -> Bool
isGoal' g = g.boss.bhp <= 0

-- Find cheapest way to win against boss
myDijkstra :: GameState -> (GameState -> [(GameState, Mana)]) -> (GameState -> Bool) -> Mana
myDijkstra start getNeighbours isGoal
  | isNothing mPath = 0
  | otherwise = manaSpent end
    where
      mPath = dijkstra queue dist prev getNeighbours isGoal
      path = fromJust mPath
      end = head . filter isGoal . M.keys $ path
      queue = H.singleton start 0 0
      dist = M.singleton start 0
      prev = M.empty 

{------------------------------{ 2nd part }------------------------------}

day22b :: String -> String
day22b = show . part2 . parseLines . lines

part2 :: Boss -> Int
part2 b = myDijkstra (GameState ogPlayer b [] 0 PlayerTurn) getNeighbours'' isGoal'

-- Give all possible GameStates from current (none if lost)
getNeighbours'' :: GameState -> [(GameState, Mana)]
getNeighbours'' g
  | g'.turn == PlayerTurn && g'.player.hp > 1 && g'.boss.bhp <= 0 = [(g', 0)]
  | g'.turn == BossTurn && g'.player.hp > 0 && g'.boss.bhp <= 0 = [(g', 0)]
  | g'.turn == PlayerTurn && g'.player.hp <= 1 || g'.player.mana < 53 = [] -- Check if player has enough mana after apply effect
  | g'.turn == BossTurn && g'.player.hp <= 0 || g'.player.mana < 53 = [] -- Check if player has enough mana after apply effect
  | g'.turn == BossTurn = map (first removeShield) (bossAttack g')
  | otherwise = map (first removeShield) (chooseSpellAndAttack looseOneHp)
  where
    g' = applyEffects g
    nb = g'.player {hp = g'.player.hp - 1}
    looseOneHp = g' { player = nb }
