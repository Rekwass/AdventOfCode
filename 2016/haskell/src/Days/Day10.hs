{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Days.Day10 (day10a, day10b) where

{------------------------------{ 1st part }------------------------------}

import Data.List (foldl', sort, find)
import qualified Data.Text as T
import qualified Data.Map as M
import Text.Parsec
import Text.Parsec.Text (Parser)
import Data.Maybe (fromJust)

-- 0, 1 or 2 values
type Values = [Int]
type Robots = M.Map Id Values

data Id = Robot Int | Output Int deriving (Eq, Ord, Show)

--                  IdB IdL IdH
type Instruction = (Id, Id, Id)

data Line' = Val (Int, Id) | Trade Instruction

day10a :: String -> String
day10a = show . part1 . parseLines . T.lines . T.pack

parseLines :: [T.Text] -> (Robots, [Instruction])
parseLines = foldl' step (M.empty, []) . map parseLine
  where
    parseLine = either (error . show) id . parse lineParser ""
    step (rs, is) = \case
      Val (val, botId) -> (M.insertWith (<>) botId [val] rs, is)
      Trade is'        -> (rs, is':is)

lineParser :: Parser Line'
lineParser = try (Val <$> valueParser) <|> Trade <$> tradeParser

valueParser :: Parser (Int, Id)
valueParser = (,)
  <$> (string "value " *> number)
  <*> (string " goes to bot " *> (Robot <$> number))

tradeParser :: Parser Instruction
tradeParser = (,,)
  <$> (string "bot " *> (Robot <$> number))
  <*> (string " gives low to " *> idParser)
  <*> (string " and high to " *> idParser)

idParser :: Parser Id
idParser =
  try (string "bot "    *> (Robot <$> number))
  <|>  string "output " *> (Output <$> number)

number :: Parser Int
number = read <$> many1 digit

part1 :: (Robots, [Instruction]) -> Int
part1 = getHim . fst . head . dropWhile notCmp61and17 . iterate step
  where
    step (rs, is) = (foldr (updateRobot is) rs twoValRobots, is)
      where
        twoValRobots = M.keys $ M.filter ((==2) . length) rs

updateRobot :: [Instruction] -> Id -> Robots -> Robots
updateRobot is botId rs = M.union rs' rs 
  where
    (_, bl, bh) = fromJust $ find (\(iid, _, _) -> iid == botId) is
    bg  = M.findWithDefault [] botId rs
    bl' = minimum bg : M.findWithDefault [] bl rs
    bh' = maximum bg : M.findWithDefault [] bh rs
    rs' = M.fromList [(botId, []), (bl, bl'), (bh, bh')]

getHim :: Robots -> Int
getHim rs = botId
  where
    Robot botId = head $ M.keys $ M.filterWithKey isGood rs

notCmp61and17 :: (Robots, [Instruction]) -> Bool
notCmp61and17 (rs, _) = M.null $ M.filterWithKey isGood rs

isGood :: Id -> Values -> Bool
isGood (Output _) _ = False
isGood (Robot _) ns = sort ns == [17, 61]

{------------------------------{ 2nd part }------------------------------}

day10b :: String -> String
day10b = show . part2 . parseLines . T.lines . T.pack

part2 :: (Robots, [Instruction]) -> Int
part2 = get012 . fst . head . dropWhile robotHas2val . iterate step
  where
    robotHas2val = not . M.null . M.filter ((==2) . length) . fst
    step (rs, is) = (foldr (updateRobot is) rs twoValRobots, is)
      where
        twoValRobots = M.keys $ M.filter ((==2) . length) rs

get012 :: Robots -> Int
get012 rs = product $ zero <> one <> two
  where
    zero = rs M.! Output 0
    one = rs M.! Output 1
    two = rs M.! Output 2
