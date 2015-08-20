module Main where

import Paths_cfrp (getDataDir)

main :: IO ()
main = do
  dataDir <- getDataDir
  putStrLn dataDir
