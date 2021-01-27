module Main where

import Lib
import System.Environment
import Data.Aeson

main :: IO ()
main = do
  progname <- getProgName
  (startingUsername:_) <- getArgs
  putStrLn $ "Running Neimhin's program: " ++ progname
  chain <- getChainOfHighestContributors startingUsername
  let links = generateLinks chain
  encodeFile ("json/"++(startingUsername ++ ".json") :: FilePath) links
  putStrLn $ (show (encode links))
