module Main where

import qualified Data.Text as T
import Lib
import System.Environment
import Data.Aeson
import Control.Concurrent.Thread
import qualified Control.Concurrent as Con
import Token

import           Servant.API                (BasicAuthData (..))

main :: IO ()
main = do
    blockingActions <- mapM forkIO (map getChainAndSaveToFile ["neimhin","fabpot", "andrew", "taylorotwell", "egoist", "HugoGiraudel", "ornicar", "bebraw","nelsonic"])
    wrapped_results <- sequence (map snd blockingActions)
    linkss <- mapM result wrapped_results
    let nodes = map generateNodes linkss
    encodeFile ("alltogether.json" :: FilePath) (Graph (foldl (++) [] nodes) (foldl (++) [] linkss))

getChainAndSaveToFile :: String -> IO [Link]
getChainAndSaveToFile startingUsername = do
  chain <- getChainOfHighestContributors startingUsername
  let links = generateLinks chain
  encodeFile ("json/"++(startingUsername ++ ".json") :: FilePath) links
  return links
