module Main where

import Lib
import System.Environment

main :: IO ()
main = do
  progname <- getProgName
  (authenticationName:startingUsername:_) <- getArgs
  putStrLn $ "Running Neimhin's program: " ++ progname
  neimhin'sFunc authenticationName startingUsername
