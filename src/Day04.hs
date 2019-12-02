{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverloadedStrings#-}
module Day04 where

import Types

import Data.List
import Data.List.Extra
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer
import qualified Data.Map as Map

data Date = Date Int Int Int Int Int deriving (Show, Eq, Ord)

minute (Date _ _ _ _ m) = m

data Instruction = BeginsShift Int | WakeUp | FallAsleep deriving (Show, Eq, Ord)

data Line = Line Date Instruction deriving (Show, Eq, Ord)

parser :: Parser [Line]
parser = (lineParser `endBy1` newline) <* eof

lineParser :: Parser Line
lineParser = do
  date <- dateParser
  char ' '
  instruction <- instructionParser
  return (Line date instruction)

dateParser :: Parser Date
dateParser = do
  char '['
  year <- decimal
  char '-'
  month <- decimal
  char '-'
  day <- decimal
  char ' '
  hour <- decimal
  char ':'
  minute <- decimal
  char ']'
  return (Date year month day hour minute)

instructionParser :: Parser Instruction
instructionParser = beginsShift <|> wakeUp <|> fallAsleep
  where
    beginsShift = do
      string "Guard #"
      i <- decimal
      string " begins shift"
      return (BeginsShift i)
    wakeUp = do
      string "wakes up"
      return WakeUp
    fallAsleep = do
      string "falls asleep"
      return FallAsleep

part1 :: [Line] -> Int
part1 lines = guard * minute
  where sleepRanges = interpret lines
        guard = mostSleepy sleepRanges
        minute = mostSleepedMinute guard sleepRanges

type Guard = Int

data SleepRange = SleepRange
  { guard :: Guard
  , begin :: Int
  , end :: Int
  } deriving (Show)


interpret :: [Line] -> [SleepRange]
interpret = generateRanges . sort

generateRanges (Line _ (BeginsShift guard):rest) = doit guard rest
  where
    doit guard (Line begin FallAsleep:Line end WakeUp:rest) =
      (SleepRange guard (minute begin) (minute end)) : (doit guard rest)
    doit _ (Line _ (BeginsShift guard):rest) = doit guard rest
    doit _ [] = []


mostSleepy :: [SleepRange] -> Guard
mostSleepy =
  biggest . Map.fromListWith (+) . map (\sr -> (guard sr, end sr - begin sr))

mostSleepedMinute :: Guard -> [SleepRange] -> Int
mostSleepedMinute g =
  biggest .
  Map.fromListWith (+) .
  map (, 1) .
  concatMap (\sr -> [begin sr .. end sr - 1]) . filter (\sr -> guard sr == g)


biggest :: Map.Map a Int -> a
biggest = fst . maximumOn snd . Map.toList

part2 :: [Line] -> Int
part2 = uncurry (*) . mostSleepedGuardMinute . interpret

mostSleepedGuardMinute :: [SleepRange] -> (Guard, Int)
mostSleepedGuardMinute =
  biggest . Map.fromListWith (+) . map (, 1) . concatMap guardMinutes
  where
    guardMinutes sr = [(guard sr, m) | m <- [begin sr .. end sr - 1]]
