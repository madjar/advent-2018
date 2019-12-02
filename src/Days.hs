{-# LANGUAGE RecordWildCards #-}
module Days where

import Types
import Utils
import Text.Megaparsec
import DiscoveredDays (days)


runLast = runDay (last days)

runDay :: Day -> IO()
runDay Day {..} = do
  let fullParser = parser <* eof
  input <- fetchInput number
  case part1 of
    Nothing -> parseTest fullParser input
    Just p1 -> do
      parsedInput <-
        either (fail . show) return . parse fullParser ("input-" <> show number) $
        input
      putStr "Part 1: "
      print (p1 parsedInput)
      case part2 of
        Just p2 -> do
          putStr "Part 2: "
          print (p2 parsedInput)
        Nothing -> return ()
