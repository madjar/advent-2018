{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
module Day03 where

import Types

import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer
import qualified Data.Map.Strict as Map
import Data.Ix
import Data.List
import Control.Arrow

data Claim = Claim
  { claimId :: Int
  , pos :: (Int, Int)
  , size :: (Int, Int)
  } deriving (Show)

parser :: Parser [Claim]
parser = claim `endBy1` newline
  where claim = do
          char '#'
          i <- decimal
          string " @ "
          px <- decimal
          char ','
          py <- decimal
          string ": "
          sx <- decimal
          char 'x'
          sy <- decimal
          return $ Claim i (px, py) (sx, sy)

part1 :: [Claim] -> Int
part1 = Map.size . Map.filter (\l -> length l >= 2) . positionsMap


part2 :: [Claim] -> Int
part2 = head . Map.keys . Map.filter (==1) . Map.fromListWith (max) . concatMap claimsOnPosByPos . Map.elems . positionsMap
  where claimsOnPosByPos l = [(p, length l) | p <- l]


positionsMap :: [Claim] -> Map.Map (Int, Int) [Int]
positionsMap = Map.fromListWith (++) . concatMap @[] coveredPosAndId

coveredPosAndId :: Claim -> [((Int, Int), [Int])]
coveredPosAndId c = map (, [claimId c]) $ getRange c

getRange Claim{..} = range (pos, (addPointsEx pos size))
  where addPointsEx (a, b) (c, d) = (a+c - 1, b+d - 1)
