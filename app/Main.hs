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
    let auth = BasicAuthData Token.authenticationName Token.token
    eitherUserRepos <- getRepos auth (T.pack "sasunts") 
    case eitherUserRepos of
      Left errs -> do print ("ERRORS: " ++ show errs)
      Right userRepos -> do
        allContribs <- getContribsForListOfReposThreaded auth (T.pack "sasunts") userRepos 
        print (show allContribs)
{-
  numCap <- Con.getNumCapabilities
  print numCap
  blockingActions <- mapM forkIO (map getChainAndSaveToFile ["neimhin","fabpot", "andrew", "taylorotwell", "egoist", "HugoGiraudel", "ornicar", "bebraw","nelsonic"])
  results <- sequence (snd (unzip blockingActions))
  return ()
-}
  --let links = (foldl (++) [] linkss)
  --let nodes = generateNodes links
  --encodeFile ("alltogether.json" :: FilePath) (links, nodes)

getChainAndSaveToFile :: String -> IO ()
getChainAndSaveToFile startingUsername = do
  chain <- getChainOfHighestContributors startingUsername
  let links = generateLinks chain
  encodeFile ("json/"++(startingUsername ++ ".json") :: FilePath) links
