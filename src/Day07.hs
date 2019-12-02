{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
module Day07 where

import Types

import Text.Megaparsec
import Text.Megaparsec.Char

import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Foldable

type Step = Char
data Instruction = Instruction
  { before :: Char
  , step :: Char
  } deriving (Show)

parser :: Parser [Instruction]
parser = instruction `endBy1` newline
  where
    instruction =
      Instruction <$ string "Step " <*> step <*
      string " must be finished before step " <*>
      step <*
      string " can begin."
    step = upperChar

part1 :: [Instruction] -> String
part1 = topo

topo :: [Instruction] -> [Step]
topo instructions = trav hasNoDeps adjacency incoming
  where
    adjacency =
      Map.fromListWith (<>) [(a, [b]) | Instruction a b <- instructions]
    incoming = Map.fromListWith (+) [(b, 1) | Instruction a b <- instructions]
    hasNoDeps =
      Set.fromList [a | Instruction a b <- instructions] Set.\\
      Map.keysSet incoming

trav :: Set Step -> Map Step [Step] -> Map Step Int -> [Step]
trav (Set.minView -> Just (nextReady, restReady)) adjacency incoming =
  let adjacents = fold $ Map.lookup nextReady adjacency
      restAdjacency = Map.delete nextReady adjacency
      (newIncoming, newlyReady) = decrementIncoming adjacents incoming
   in nextReady :
      trav (restReady <> Set.fromList newlyReady) restAdjacency newIncoming
trav _ _ _ = []


decrementIncoming :: (Ord k) => [k] -> Map k Int -> (Map k Int, [k])
decrementIncoming (x:xs) incoming =
  case (incoming Map.! x - 1) of
    0 ->
      let (newIncoming, result) = decrementIncoming xs (Map.delete x incoming)
       in (newIncoming, x : result)
    newValue -> decrementIncoming xs (Map.insert x newValue incoming)
decrementIncoming [] incoming = (incoming, [])
