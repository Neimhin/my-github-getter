module Main where

import Lib
import System.Environment

main :: IO ()
main = do
  progname <- getProgName
  putStrLn $ "Running Neimhin's program: " ++ progname
  neimhin'sFunc
