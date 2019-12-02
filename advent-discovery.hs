#!/usr/bin/env stack
-- stack --resolver nightly-2018-12-04 script

import System.Environment (getArgs)
import System.Directory
import System.FilePath
import Text.Printf
import Control.Monad
import Data.List


twoDigits :: Int -> String
twoDigits = printf "%02d"

makeDay :: String -> Int -> IO String
makeDay srcDir i = do
  let ii = twoDigits i
  content <- readFile (srcDir </> "Day" <> ii <> ".hs")
  let part k =
        if ("part" <> k <> " ") `isInfixOf` content
          then "(Just Day" <> ii <> ".part" <> k <> ")"
          else "nope"
  return $
    "Day " <> show i <> " Day" <> ii <> ".parser " <> part "1" <> " " <>
    part "2"

main :: IO ()
main = do
  [src, _, dst] <- getArgs
  let srcDir = takeDirectory src
  files <-
    filterM
      (\i -> doesFileExist (srcDir </> "Day" <> twoDigits i <> ".hs"))
      [1 .. 25]
  days <- mapM (makeDay srcDir) files
  writeFile dst $
    concat
      ["module " <> takeBaseName src <> " (days) where\n"
      , "import Types\n"
      , unlines ["import qualified Day" <> twoDigits i | i <- files]
      , "\n"
      , "nope :: Maybe ( a -> ())\n"
      , "nope = Nothing\n"
      , "days = ["
      , intercalate "\n  ," days
      , "]\n"
      ]
