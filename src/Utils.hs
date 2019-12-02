{-# LANGUAGE OverloadedStrings #-}
module Utils where

import Network.HTTP.Simple
import Data.Text.Lazy as Text
import Data.Text.Lazy.IO as Text
import Data.Text.Lazy.Encoding (decodeUtf8)
import System.Directory
import System.FilePath
import qualified Data.Set as Set

inputsDir = "input"

fetchInput :: Int -> IO Text
fetchInput n = do
  createDirectoryIfMissing False inputsDir
  let inputFile = inputsDir </> show n
  alreadyDownloaded <- doesFileExist inputFile
  if alreadyDownloaded
    then Text.readFile inputFile
    else do
      request <-
        parseRequestThrow
          ("https://adventofcode.com/2018/day/" <> show n <> "/input")
      response <-
        httpLBS
          (addRequestHeader
             "Cookie"
             "session=XXX"
             request)
      let input = decodeUtf8 (getResponseBody response)
      Text.writeFile inputFile input
      return input


firstDup :: Ord a => [a] -> a
firstDup = go mempty
  where go set (x:xs) = if Set.member x set
                          then x
                          else go (Set.insert x set) xs

