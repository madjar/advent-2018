{-# LANGUAGE ExistentialQuantification #-}
module Types where

import Data.Text.Lazy (Text)
import Text.Megaparsec
import Data.Void

type Parser a = Parsec Void Text a

data Day = forall o1 o2 i. (Show o1, Show o2, Show i) =>
                           Day
  { number :: Int
  , parser :: Parser i
  , part1 :: Maybe (i -> o1)
  , part2 :: Maybe (i -> o2)
  }

