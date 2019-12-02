module Day02 where

import Utils
import Types
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Data.Map.Strict as Map
import Data.List
import qualified Control.Foldl as L

parser :: Parser [[Char]]
parser = some lowerChar `endBy1` newline

part1 :: [[Char]] -> Int
part1 = L.fold part1F
  where part1F = L.premap countLetters ((*) <$> hasF 2 <*> hasF 3)

countLetters :: [Char] -> Map.Map Char Int
countLetters = foldl1' (Map.unionWith (+)) . map (flip Map.singleton 1)

has :: Int -> Map.Map Char Int -> Bool
has n = not . Map.null . Map.filter (==n)

hasF :: Int -> L.Fold (Map.Map Char Int) Int
hasF n = L.prefilter (has n) L.length

part2 :: [String] -> String
part2 = filter (/= '_') . firstDup . concatMap indexes

indexes :: String -> [String]
indexes = go []
  where go head (x:xs) = (head ++ "_" ++xs): go (head ++ [x]) xs
        go _ [] = []
