module Main where

import Lib
import System.Environment
import Data.Aeson
import Control.Concurrent



main :: IO ()
main = do
  threadIds <- mapM forkIO (map getChainAndSaveToFile ["neimhin","fabpot", "andrew", "taylorotwell", "egoist", "HugoGiraudel", "ornicar", "bebraw","nelsonic"])
  return ()
  --let links = (foldl (++) [] linkss)
  --let nodes = generateNodes links
  --encodeFile ("alltogether.json" :: FilePath) (links, nodes)

getChainAndSaveToFile :: String -> IO ()
getChainAndSaveToFile startingUsername = do
  chain <- getChainOfHighestContributors startingUsername
  let links = generateLinks chain
  encodeFile ("json/"++(startingUsername ++ ".json") :: FilePath) links
