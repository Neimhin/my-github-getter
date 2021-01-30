{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DuplicateRecordFields     #-}
{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}

module Lib where

import qualified GitHub as GH
import qualified Servant.Client as SC
import           System.Environment           (getArgs)
import           System.IO (hFlush, stdout)
import Data.Text (Text, pack, unpack)
import Data.List (intercalate, groupBy, sortBy)
import Data.Either (partitionEithers)
import           Servant.API                (BasicAuthData (..))
import Data.ByteString.UTF8 (fromString)

import Control.Concurrent.Thread
import qualified Control.Concurrent as Con

import GHC.Generics ( Generic )
import qualified Data.Aeson as Aeson

-- To use this program you must authenticate your API calls.
-- Create a module in src/Token.hs. Make sure git is ignoring that file
-- `echo src/Token.hs >> .gitignore`
-- The module should have the following two lines and nothing else: (Include the double quotes but not the angle brackets)
{-
module Token where
token = "<your-token-here>"
-}
import qualified Token

data Graph = Graph {
    nodes :: [Node],
    links :: [Link]
} deriving (Generic, Aeson.ToJSON, Show)

data Node = Node {
    id :: Text,
    value :: Integer
} deriving (Generic, Aeson.ToJSON, Show)

data Link = Link {
    source :: Text
  , target :: Text
  , source_value :: Integer
  , target_value :: Integer
} deriving (Generic, Aeson.ToJSON, Show)

log :: String -> IO ()
log content = do
  id <- Con.myThreadId
  appendFile ("thread-" ++ (show id) ++ ".log" :: FilePath) content

generateNodes :: [Link] -> [Node]
generateNodes xs = generateNodes' [] xs

generateNodes' :: [Node] -> [Link] -> [Node]
generateNodes' found [] = found
generateNodes' found ((Link new _ new_value _):xs) 
    | newTo found new  = generateNodes' ((Node new new_value):found) xs
    | otherwise = generateNodes' found xs

newTo :: [Node] -> Text -> Bool
newTo [] _ = True
newTo ((Node id _):xs) id' | id == id' = False
                         | otherwise = newTo xs id'

generateLinks :: [GH.RepoContributor] -> [Link]
generateLinks chain = (generateLinks' [] chain chain)

generateLinks' :: [Link] -> [GH.RepoContributor] -> [GH.RepoContributor] -> [Link] 
generateLinks' found [] _ = found 
generateLinks' found (next@(GH.RepoContributor source source_value):[]) chain = ((Link source target source_value target_value):found)
    where
      lastTarget' = lastTarget next chain
      target = fst lastTarget'
      target_value = snd lastTarget'
      lastTarget s@(GH.RepoContributor n v) [] =  (n, v)
      lastTarget s@(GH.RepoContributor n v) ((GH.RepoContributor n' v'):cs) | n == n' = (n, v)
                                                                            | otherwise = lastTarget s cs

generateLinks' found ((GH.RepoContributor source source_value):b@(GH.RepoContributor target target_value):xs) chain =
  generateLinks' ((Link source target source_value target_value):found) (b:xs) chain

type Username = [Char]
getChainOfHighestContributors :: Username -> IO [GH.RepoContributor]
getChainOfHighestContributors username = do
    -- token and authentictaionName are imported from Token.hs (remember to make sure git is ignoring Token.hs)
    let auth = BasicAuthData Token.authenticationName Token.token
    -- the starting username is the first node in the chain but the number of contributions is set to 0
    reversechain <- neimhin'sFunc [(GH.RepoContributor (pack username) 0)] username auth
    return (reverse reversechain)

reachedLoop :: [GH.RepoContributor] -> Bool
reachedLoop [] = False
reachedLoop ((GH.RepoContributor n _):rs) = reachedLoop' n rs

reachedLoop' _ [] = False
reachedLoop' n ((GH.RepoContributor n' _):rs) | n == n' = True
                       | otherwise = reachedLoop' n rs

neimhin'sFunc :: [GH.RepoContributor] -> Username -> BasicAuthData -> IO [GH.RepoContributor]
neimhin'sFunc chain username auth = do
  case (reachedLoop chain) of
    True -> do
      putStrLn ("chain found: " ++ show chain)
      return chain 
    False -> do
      putStrLn $ "trying to get " ++ username ++ "'s repos"
      userRepos <- (getRepos auth (pack username))
      case userRepos of
        Left err -> do putStrLn $ "error getting list of repos for " ++ username ++ ":\n" ++ (show err)
                       return chain
        Right listOfRepos -> do
          putStrLn $ username ++ "'s repos are: " ++ intercalate ", " (map (\(GH.GitHubRepo n _ _) -> unpack n) listOfRepos)
          fmap partitionEithers (mapM (getContribs auth (pack username)) listOfRepos) >>= \case
            (errs, listOfListsOfContributors) -> do
              case errs of 
                  (x:_) -> putStrLn $ "some errors were found:\n" ++ (show errs)
                  [] -> putStrLn $ "smooth sailing getting listOfListsOfContributors for " ++ username
              putStrLn $ "calculating highest contributor in " ++ username ++ "'s repos"
              case (getHighest (getJusts (map getHighest listOfListsOfContributors))) of
                Nothing -> do putStrLn "no highest contributor found. terminating now"
                              return chain
                Just highest@(GH.RepoContributor highest_login _) -> do 
                  putStrLn $ "the highest contributor in " ++ username ++ "'s repos is " ++ show highest
                  putStrLn $ "executing `neimhin'sFunc` for " ++ show highest
                  result <- neimhin'sFunc (highest:chain) (unpack (highest_login)) auth
                  return result
  where 
        getContribs :: BasicAuthData -> GH.Username -> GH.GitHubRepo -> IO (Either SC.ClientError [GH.RepoContributor])
        getContribs auth name (GH.GitHubRepo repo _ _) = do
          putStr $ ((show repo) ++ ":\n")
          hFlush stdout
          GH.runClientMPaged (GH.getRepoContribs (Just "haskell-app") auth name repo)
        
        getJusts :: [Maybe a] -> [a]
        getJusts x = getJusts' [] x
        getJusts' :: [a] -> [Maybe a] -> [a]
        getJusts' xs [] = xs
        getJusts' xs (Nothing:as) = getJusts' xs as
        getJusts' xs (Just a :as) = getJusts' (a:xs) as   

getContribsForListOfReposThreaded :: BasicAuthData -> Text -> [GH.GitHubRepo] -> IO [GH.RepoContributor]
getContribsForListOfReposThreaded auth name list = do
  threadBlockingActions <- mapM forkIO (map GH.runClientMPaged (map apiCall listOfRepos))
  wrappedResults <- (sequence (map snd threadBlockingActions))
  results <- mapM result wrappedResults
  let (errs, rs) = partitionEithers results
  putStrLn ( show errs )
  return (foldl (++) [] rs)
    where
      listOfRepos = map GH.getName list
      apiCall = (\reponame -> GH.getRepoContribs (Just "haskell-app") auth name reponame) 

highestOf Nothing new = new
highestOf (Just old) (Just new) = case (compare old' new') of 
     LT -> (Just new)
     otherwise -> (Just old)
   where
      old' = GH.contributions old
      new' = GH.contributions new

getRepos :: BasicAuthData -> GH.Username -> IO (Either SC.ClientError [GH.GitHubRepo])
getRepos auth username = GH.runClientMPaged (GH.getUserRepos (Just "haskell-app") auth username)
 
getRepoContribs' auth = runGetRepoContribs "haskell-app" auth "Neimhin" "Catch2"
      >>= \x -> case x of
       Left err -> do
         putStrLn $ "Error while trying GH.getUserRepos " ++ show err
       Right result -> do
         putStrLn $ "hooray\n" ++ show result 

getHighest :: [GH.RepoContributor] -> Maybe GH.RepoContributor
getHighest [] = Nothing
getHighest (contributor:list) = getHighest' contributor list

getHighest' :: GH.RepoContributor -> [GH.RepoContributor] -> Maybe GH.RepoContributor
getHighest' highestSoFar [] =  Just highestSoFar
getHighest' highestSoFar (contributor:list) = case (compare (GH.contributions contributor) (GH.contributions highestSoFar)) of
       LT -> getHighest' highestSoFar list
       otherwise -> getHighest' contributor list


runGetRepoContribs userAgent auth username reponame = GH.runClientMPaged $ GH.getRepoContribs (Just userAgent) auth (pack username) (pack reponame)

{-
getHighestContributor :: BasicAuthData -> [GH.GitHubRepo] ->  Maybe GH.RepoContributor -> IO ()
getHighestContributor auth repos = getHighestContributor' auth repos Nothing

getHighestContributor' :: BasicAuthData -> [GH.GitHubRepo] -> Maybe GH.RepoContributor -> IO ()
getHighestContributor' _ [] highest = highest
getHighestContributor' auth (gitHubRepo:xs) highest = do
  let getRepoContribsResult = env >>= (SC.runClientM (GH.getRepoContribs (Just "haskell-app") auth (pack "Neimhin") (GH.ownername gitHubRepo)))
  getRepoContribsResult >>= \case 
     Left err -> do putStrLn $ "Goofs on getRepoContribs: " ++ show err
     Right result -> case (getHighest result) of 
                       Nothing -> getHighestContributor' auth xs highest
                       highest' -> getHighestContributor' auth xs (highestOf highest highest')
-}


{-
getHighestContributorToCatch2 auth = do 
  manager' <- newManager tlsManagerSettings
  let environment = do return $ SC.mkClientEnv manager' (SC.BaseUrl SC.Http "api.github.com" 80 "")
  let getRepoContribsResult = environment >>= (SC.runClientM (GH.getRepoContribs (Just "haskell-app") auth (pack "Neimhin") (pack "Catch2")))
  getRepoContribsResult >>= \case 
     Left err -> do putStrLn $ "Goofs on getRepoContribs: " ++ show err
     Right result -> case (getHighest result) of 
                       Nothing -> putStrLn "No contributors found"
                       Just highest -> putStrLn $ "The highest contributor to Catch2 is: " ++ show highest

getNeimhin auth =  (SC.runClientM (GH.getUser (Just "haskell-app") auth (pack "Neimhin")) =<< env) 
            >>= \case
               Left err -> do
                    putStrLn $ "Error while trying GH.getUser" ++ show err
               Right result -> do
                    putStrLn $ show result
-}

{- 
getNeimhin'sRepos auth = (SC.runClientM (GH.getUserRepos (Just "haskell-app") auth (pack "Neimhin")) =<< env) 
   >>= \case
    Left err -> do
      putStrLn $ "Error while trying GH.getUserRepos " ++ show err
    Right result -> do
      putStrLn $ "hooray\n" ++ show result 
-}


{-
getAuthenticatedUser'sIssues auth = (SC.runClientM (GH.getUserIssues (Just "haskell-app") auth) =<< env) 
   >>= \case
    Left err -> do
      putStrLn $ "\n\nError while trying GH.getUserIssues " ++ show err ++ "\n\n"
    Right result -> do
      putStrLn $ "\n\nhooray, here are the authenticated user's Issues\n\n" ++ show result 


getCatch2Issues auth = (SC.runClientM (GH.getIssues (Just "haskell-app") auth (pack "catchorg") (pack "Catch2")) =<< env) 
   >>= \case
    Left err -> do
      putStrLn $ "\n\nError while trying GH.getIssues " ++ show err ++ "\n\n"
    Right result -> do
      putStrLn $ "\n\nhooray, here are Neimhin's Issues on Catch2\n" ++ show result 
-}



{-
gitHubProgram :: IO ()
gitHubProgram = do
  putStrLn "Let's try a GitHubCall"
  (rName:user:_) <- getArgs
  putStrLn $ "name is " ++ rName
  putStrLn $ "github account for API call is " ++ user
  putStrLn $ "github token for api call is in Token.hs"

  let auth = BasicAuthData (fromString user) token
  
  testGitHubCall auth $ pack rName
  putStrLn "end."


testGitHubCall :: BasicAuthData -> Text -> IO ()
testGitHubCall auth name = 
  (SC.runClientM (GH.getUser (Just "haskell-app") auth name) =<< env) >>= \case

    Left err -> do
      putStrLn $ "heuston, we have a problem: " ++ show err
    Right result -> do
      putStrLn $ "the votes of the github jury are " ++ show result
      
      -- now lets get the users repositories
      (SC.runClientM (GH.getUserRepos (Just "haskell-app") auth name) =<< env) >>= \case
        Left err -> do
          putStrLn $ "heuston, we have a problem (getting repos): " ++ show err
        Right repos -> do
          putStrLn $ " repositories are:" ++
            intercalate ", " (map (\(GH.GitHubRepo n _ _ ) -> unpack n) repos)

          -- now lets get the full list of collaborators from repositories
          partitionEithers `fmap` mapM (getContribs auth name) repos >>= \case

            ([], contribs) ->
              putStrLn $ " contributors are: " ++
              (intercalate "\n\t" .
               map (\(GH.RepoContributor n c) -> show n ++ ",\t\t" ++ show c) .
               groupContributors $ concat contribs)

            (ers, _)-> do
              putStrLn $ "heuston, we have a problem (getting contributors): " ++ show ers
          
                
  where env :: IO SC.ClientEnv
        env = do
          manager <- newManager tlsManagerSettings
          return $ SC.mkClientEnv manager (SC.BaseUrl SC.Http "api.github.com" 80 "")

        getContribs :: BasicAuthData -> GH.Username -> GH.GitHubRepo -> IO (Either SC.ClientError [GH.RepoContributor])
        getContribs auth name (GH.GitHubRepo repo _ _) = do
          putStrLn $ "Getting contributors for " ++ show repo
          SC.runClientM (GH.getRepoContribs (Just "haskell-app") auth name repo) =<< env

        groupContributors :: [GH.RepoContributor] -> [GH.RepoContributor]
        groupContributors  = sortBy (\(GH.RepoContributor _ c1) (GH.RepoContributor _ c2) -> compare c1 c2) .
                             map mapfn .
                             groupBy (\(GH.RepoContributor l1 _) (GH.RepoContributor l2 _) -> l1 == l2)
         where mapfn :: [GH.RepoContributor] -> GH.RepoContributor
               mapfn xs@((GH.RepoContributor l _):_) = GH.RepoContributor l .sum $ 
                                                       map (\(GH.RepoContributor _ c) -> c)  xs
               
-}       
