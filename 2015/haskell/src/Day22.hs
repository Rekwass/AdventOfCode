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
part1 b = myDijkstra (GameState ogPlayer b [] 0 PlayerTurn) nextGameState isWon

-- Give all possible GameStates from current (none if lost)
nextGameState :: GameState -> [(GameState, Mana)]
nextGameState = removeEffects . playTurn . applyEffects

applyEffects :: GameState -> GameState
applyEffects (GameState p b ss m t) = GameState p' b' ss' m t
  where
    -- (GameState p' b' ss' _ _) = removeShield g
    (p', b', ss') = foldr applyEffect (p, b, []) ss

playTurn :: GameState -> [(GameState, Mana)]
playTurn g
  | g.turn == BossTurn = bossAttack g
  | otherwise          = playerAttack g

playerAttack :: GameState -> [(GameState, Mana)]
playerAttack g
  | isWon g = [(g, 0)]
  | isLost g = [] -- Check if player has enough mana after apply effect
  | otherwise = chooseSpellAndAttack g

removeEffects :: [(GameState, Mana)] -> [(GameState, Mana)]
removeEffects = map removeShield

removeShield :: (GameState, Mana) -> (GameState, Mana)
removeShield gm@(g, m)
  | any isShield g.effects = gm
  | otherwise              = (g { player = p' }, m)
    where
      p' = g.player { armour = 0 }

isShield :: (Turn, Spell) -> Bool
isShield (_, Shield {}) = True
isShield _              = False

applyEffect :: (Turn, Spell) -> (Player, Boss, [(Turn, Spell)]) -> (Player, Boss, [(Turn, Spell)])
applyEffect (t, s@(Shield _ _ arm)) (p, b, nss)
  | t == 1    = (p { armour = arm } , b, nss)
  | otherwise = (p { armour = arm}, b, (t - 1, s):nss)
applyEffect (t, s@(Poison _ _ dmg)) (p, b, nss)
  | t == 1    = (p, b { bhp = b.bhp - dmg}, nss)
  | otherwise = (p, b { bhp = b.bhp - dmg}, (t - 1, s):nss)
applyEffect (t, s@(Recharge _ _ m)) (p, b, nss)
  | t == 1    = (p { mana = p.mana + m }, b, nss)
  | otherwise = (p { mana = p.mana + m }, b, (t - 1, s):nss)
applyEffect _ _ = error "Could not apply effect ?"

chooseSpellAndAttack :: GameState -> [(GameState, Mana)]
chooseSpellAndAttack g = map (castSpell g) availableSpells
  where
    availableSpells              = filter hasMana . filter (not . isActive) $ spells
    isActive s                   = s `elem` map snd g.effects
    hasMana (MagicMissile m _)   = g.player.mana >= m
    hasMana (Drain        m _ _) = g.player.mana >= m
    hasMana (Shield       m _ _) = g.player.mana >= m
    hasMana (Poison       m _ _) = g.player.mana >= m
    hasMana (Recharge     m _ _) = g.player.mana >= m

castSpell :: GameState -> Spell -> (GameState, Mana)
castSpell g sp = (apply sp g, cost sp)
  where
    cost (MagicMissile m _)   = m
    cost (Drain        m _ _) = m
    cost (Shield       m _ _) = m
    cost (Poison       m _ _) = m
    cost (Recharge     m _ _) = m

    spendMana m p = p { mana = p.mana - m }

    apply (MagicMissile m dmg)     g' = g' { player = spendMana m g'.player
                                           , boss = g'.boss { bhp = g'.boss.bhp - dmg }
                                           , manaSpent = g'.manaSpent + m
                                           , turn = BossTurn }
    apply (Drain        m dmg dhp) g' = g' { player = (spendMana m g'.player) { hp = g'.player.hp + dhp }
                                           , boss = g'.boss { bhp = g'.boss.bhp - dmg }
                                           , manaSpent = g'.manaSpent + m
                                           , turn = BossTurn }
    apply s@(Shield     m t   _)  g' = addEffect m t s g'
    apply s@(Poison     m t   _)  g' = addEffect m t s g'
    apply s@(Recharge   m t   _)  g' = addEffect m t s g'

    addEffect m t s g' = g' { player = spendMana m g'.player
                            , manaSpent = g'.manaSpent + m
                            , effects = (t, s) : g'.effects
                            , turn = BossTurn }

bossAttack :: GameState -> [(GameState, Mana)]
bossAttack g
  | isWon        g  = [(g, 0)]
  | isPlayerDead g' = []
  | otherwise       = [(g', 0)]
  where
    p  = g.player { hp = g.player.hp - max (g.boss.damage - g.player.armour) 1 }
    g' = g { player = p, turn = PlayerTurn }

-- True if player alive and boss dead
isWon :: GameState -> Bool
isWon g = g.player.hp > 0 && g.boss.bhp <= 0

isLost :: GameState -> Bool
isLost g = g.player.hp <= 0 || g.player.mana < 53 -- 53 is the lowest mana cost to cast a spell

isPlayerDead :: GameState -> Bool
isPlayerDead g = g.player.hp <= 0

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
part2 b = myDijkstra (GameState ogPlayer b [] 0 PlayerTurn) nextGameState' isWon

nextGameState' :: GameState -> [(GameState, Mana)]
nextGameState' = removeEffects . playTurn . hitPlayer . applyEffects

hitPlayer :: GameState -> GameState
hitPlayer g
  | g.turn == PlayerTurn = g { player = p' }
  | otherwise            = g
  where
    p' = g.player { hp = g.player.hp - 1 }
