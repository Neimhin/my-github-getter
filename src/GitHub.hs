{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE DuplicateRecordFields     #-}

module GitHub where 

import           Control.Monad       (mzero)
import           Data.Aeson
import           Data.Proxy
import           Data.Text
import           GHC.Generics
import           Network.HTTP.Client (defaultManagerSettings, newManager)
import           Servant.API
import           Servant.Client

type Username  = Text
type UserAgent = Text
type Reponame  = Text
type Userlogin = Text
type Owner = Text


data GitHubUser =
  GitHubUser { login :: Text
             , name  :: Text
             , email :: Maybe Text
             } deriving (Generic, FromJSON, Show)

data GitHubRepo =
  GitHubRepo { name :: Text
             , fullname :: Maybe Text
             , language :: Maybe Text
             } deriving (Generic, FromJSON, Show)

data RepoContributor =
  RepoContributor { login :: Text
                  , contributions :: Integer
                  } deriving (Generic, FromJSON, Show)

data Login =
  Login { login :: Text }
        deriving (Generic, FromJSON, Show)

data GitHubIssue =
  GutHubIssue { title :: Text,
           body  :: Text }
        deriving (Generic, FromJSON, Show)


type GitHubAPI = 
            -- GET /users/:username
                 Header  "user-agent" UserAgent
              :> BasicAuth "github" Int 
              :> "users" :> Capture "username" Username  
              :> Get '[JSON] GitHubUser
              
            -- GET /users/:username/repo             
            :<|> Header  "user-agent" UserAgent
              :> BasicAuth "github" Int 
              :> "users" :> Capture "username" Username  :> "repos" 
              :> Get '[JSON] [GitHubRepo]
            
            -- GET /repos/:username/:repo/contributors             
            :<|> Header  "user-agent" UserAgent
              :> BasicAuth "github" Int 
              :> "repos" :> Capture "username" Username :> Capture "repo" Reponame :> "contributors" 
              :>  Get '[JSON] [RepoContributor]
            
            -- GET /repos/:owner/:repo/issues            
            :<|> Header "user-agent" UserAgent
              :> BasicAuth "github" Int
              :> "repos" :> Capture "owner" Owner :> Capture "repo" Reponame :> "issues"
              :> Get '[JSON] GitHubIssue

            -- GET /user/issues
            -- (get issues assigned to the authenticated user)
            :<|> Header "user-agent" UserAgent
              :> BasicAuth "github" Int
              :> "user" :> "issues" 
              :> Get '[JSON] [GitHubIssue]

gitHubAPI :: Proxy GitHubAPI
gitHubAPI = Proxy

getUser ::          Maybe UserAgent -> BasicAuthData -> Username            -> ClientM GitHubUser
getUserRepos ::     Maybe UserAgent -> BasicAuthData -> Username            -> ClientM [GitHubRepo]
getRepoContribs ::  Maybe UserAgent -> BasicAuthData -> Username -> Reponame -> ClientM [RepoContributor]
getIssues ::        Maybe UserAgent -> BasicAuthData -> Owner -> Reponame -> ClientM GitHubIssue
getUserIssues ::    Maybe UserAgent -> BasicAuthData -> ClientM [GitHubIssue]


-- Build the functions
getUser :<|> getUserRepos :<|> getRepoContribs :<|> getIssues :<|> getUserIssues = client gitHubAPI

