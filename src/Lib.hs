{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE DeriveAnyClass       #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DuplicateRecordFields     #-}

module Lib
    ( gitHubProgram
     ,neimhin'sFunc
    ) where

import qualified GitHub as GH
import qualified Servant.Client               as SC
import           Network.HTTP.Client          (newManager)
import           Network.HTTP.Client.TLS      (tlsManagerSettings)
import           System.Environment           (getArgs)
import Data.Text hiding (map,intercalate, groupBy, concat)
import Data.List (intercalate, groupBy, sortBy)
import Data.Either
import           Servant.API                (BasicAuthData (..))
import Data.ByteString.UTF8 (fromString)

-- To use this program you must authenticate your API calls.
-- Create a module in src/Token.hs. Make sure git is ignoring that file
-- `echo src/Token.hs >> .gitignore`
-- The module should have the following two lines and nothing else: (Include the double quotes but not the angle brackets)
{-
module Token where
token = "<your-token-here>"
-}
import Token

neimhin'sFunc :: IO ()
neimhin'sFunc = do
  putStrLn "Functions written by Neimhin:\n"
  (authenticationName:_) <- getArgs
  -- token has been imported from Token.hs
  let auth = BasicAuthData (fromString authenticationName) (fromString token) 
  manager' <- newManager tlsManagerSettings
  let environment = do return $ SC.mkClientEnv manager' (SC.BaseUrl SC.Http "api.github.com" 80 "")
  let getRepoContribsResult = environment >>= (SC.runClientM (GH.getRepoContribs (Just "haskell-app") auth (pack "Neimhin") (pack "Catch2")))
  getRepoContribsResult >>= \case 
     Left err -> do putStrLn $ "Goofs on getRepoContribs: " ++ show err
     Right result -> case (getHighest result) of 
                       Nothing -> putStrLn "No contributors found"
                       Just highest -> putStrLn $ "The highest contributor was: " ++ show highest

  (SC.runClientM (GH.getUser (Just "haskell-app") auth (pack "Neimhin")) =<< env) 
            >>= \case
               Left err -> do
                    putStrLn $ "Error while trying GH.getUser" ++ show err
               Right result -> do
                    putStrLn $ show result
  (SC.runClientM (GH.getUserRepos (Just "haskell-app") auth (pack "Neimhin")) =<< env) 
   >>= \case
    Left err -> do
      putStrLn $ "Error while trying GH.getUserRepos " ++ show err
    Right result -> do
      putStrLn $ "hooray\n" ++ show result 

  env >>= runGetRepoContribs "haskell-app" auth "Neimhin" "Catch2"
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


env :: IO SC.ClientEnv
env = do
  manager <- newManager tlsManagerSettings
  return $ SC.mkClientEnv manager (SC.BaseUrl SC.Http "api.github.com" 80 "")

runGetRepoContribs userAgent auth username reponame = SC.runClientM $ GH.getRepoContribs (Just userAgent) auth (pack username) (pack reponame)

gitHubProgram :: IO ()
gitHubProgram = do
  putStrLn "Let's try a GitHubCall"
  (rName:user:_) <- getArgs
  putStrLn $ "name is " ++ rName
  putStrLn $ "github account for API call is " ++ user
  putStrLn $ "github token for api call is " ++ token

  let auth = BasicAuthData (fromString user) (fromString token)
  
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
          putStrLn $ "heuston, we have a problem (gettign repos): " ++ show err
        Right repos -> do
          putStrLn $ " repositories are:" ++
            intercalate ", " (map (\(GH.GitHubRepo n _ _ ) -> unpack n) repos)

          -- now lets get the full list of collaborators from repositories
          partitionEithers <$> mapM (getContribs auth name) repos >>= \case

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
               
              
                
          



       
