{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverloadedStrings #-}
module Day06 where

import Types

import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer

import Data.Ix
import Data.List.Extra
import Control.Arrow
import Data.Ord
import qualified Data.Set as Set
import qualified Data.Map as Map
import Data.Maybe

parser :: Parser [(Int, Int)]
parser = location `endBy1` newline
  where location = (,) <$> decimal <* string ", " <*> decimal

manhattan (x, y) (z,t) = abs (x - z) + abs (y - t)

gridMin locations = (minimum $ map fst locations, minimum $ map snd locations)
gridMax locations = (maximum $ map fst locations, maximum $ map snd locations)
grid locations = range (gridMin locations, gridMax locations)

part1 locations = solution
  where
    closestLocationFromPoint point = lonelyMinimumOn (manhattan point) locations
    locationArea =
      Map.fromListWith (+) .
      map (, 1) . catMaybes . map closestLocationFromPoint $
      grid locations
      -- XXX remove the one that have infinite area (is that have point in the
      -- outer border in their area)
    locationThatTouchOuterBorder =
      Set.fromList . catMaybes . map closestLocationFromPoint $
      outerBorder (gridMin locations) (gridMax locations)
    solution =
      snd .
      maximumOn snd .
      filter (\(l, d) -> l `Set.notMember` locationThatTouchOuterBorder) .
      Map.toList $
      locationArea

lonelyMinimumOn
  :: (Ord a1, Ord a2) => (a2 -> a1) -> [a2] -> Maybe a2
lonelyMinimumOn f l =
  case sort . map (f &&& id) $ l of
    x:y:_
      | fst x == fst y -> Nothing
    (_, x):_ -> Just x

outerBorder (xmin, ymin) (xmax, ymax) =
  range ((xmin - 1, ymin - 1), (xmax + 1, ymin - 1))
  ++ range ((xmin - 1, ymax + 1), (xmax + 1, ymax + 1))
  ++ range ((xmin - 1, ymin - 1), (xmin - 1, ymax + 1))
  ++ range ((xmax + 1, ymin - 1), (xmax + 1, ymax + 1))

part2 :: [(Int, Int)] -> Int
part2 = solvePart2 10000

solvePart2 n locations =
  length . filter (< n) . map totalDistanceToLocations . grid $ locations
  where
    totalDistanceToLocations p = sum . map (manhattan p) $ locations
