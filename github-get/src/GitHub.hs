{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE DuplicateRecordFields     #-}
{-# LANGUAGE OverloadedStrings    #-}


module GitHub where

import Data.Aeson ( FromJSON )
import Data.Proxy
import Data.Text 
import GHC.Generics
import Servant.Client         
import Servant.API
import Control.Monad    (mzero)
import           Network.HTTP.Client          (defaultManagerSettings, newManager)


type Username  = Text
type UserAgent = Text
type Reponame  = Text

data GitHubUser =
  GitHubUser { name  :: Text
             , followers :: Int
             , following :: Int
             } deriving (Generic, FromJSON, Show)

data GitHubTopics =
   GitHubTopics { subscribers_count :: Int
                } deriving (Generic, FromJSON, Show)

type GitHubAPI = "users" :> Header  "user-agent" UserAgent
                         :> Capture "username" Username  :> Get '[JSON] GitHubUser

            :<|> "repos" :> Header  "user-agent" UserAgent
                         :> Capture "owner" Username  :> Capture "repo" Reponame :> Get '[JSON] GitHubTopics

gitHubAPI :: Proxy GitHubAPI
gitHubAPI = Proxy


getUser ::          Maybe UserAgent -> Username            -> ClientM GitHubUser
getRepo :: Maybe UserAgent -> Username -> Reponame -> ClientM GitHubTopics


getUser :<|> getRepo = client gitHubAPI

