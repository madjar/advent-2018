module Day01 where

import Types
import Utils
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer

import qualified Data.Text.Lazy as T

part1 :: [Int] -> Int
part1 = sum

part2 :: [Int] -> Int
part2 = firstDup . scanl1 (+) . cycle

parser :: Parser [Int]
parser = some (lexeme space1 (signed mempty decimal))
