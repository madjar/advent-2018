module Day05 where

import Types

import Data.Char
import Text.Megaparsec
import Text.Megaparsec.Char

data Elem = Elem
  { symbol :: Char
  , polarity :: Bool
  } deriving (Show)

mkElem :: Char -> Elem
mkElem char = Elem (toLower char) (isUpper char)

reacts :: Elem -> Elem -> Bool
reacts (Elem c1 b1) (Elem c2 b2) = c1 == c2 && b1 /= b2

parser :: Parser [Elem]
parser = many elem <* newline
  where elem = mkElem <$> letterChar

part1 :: [Elem] -> Int
part1 = length . reduce

part2 :: [Elem] -> Int
part2 input =
  minimum .
  map (length . reduce) . map (\l -> filter (\e -> symbol e /= l) input) $
  ['a' .. 'z']

reduce :: [Elem] -> [Elem]
reduce (x:xs) =
  case reduce xs of
    y:ys
      | x `reacts` y -> ys
    ys -> x : ys
reduce [] = []
