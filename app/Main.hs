module Main where

import Lib
import System.Environment

main :: IO ()
main = do
  progname <- getProgName
  (startingUsername:_) <- getArgs
  putStrLn $ "Running Neimhin's program: " ++ progname
  chain <- getChainOfHighestContributors startingUsername
  putSrLn $ show chain
