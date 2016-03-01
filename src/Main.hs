{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeOperators         #-}


module Main where

import           Control.Monad.Trans.Either
import           Data.Aeson
import           Data.Text
import           GHC.Generics
import           Network.Wai
import           Network.Wai.Handler.Warp
import           Servant
import           Servant.API

type IssueAPI =
  Get '[JSON] [Issue]
  :<|> Capture "issueId" Int :> Get '[JSON] Issue

type API = "issues" :> IssueAPI

data Issue = Issue {
    issueId :: Int
  , title   :: String
  , body    :: String
} deriving (Eq, Show, Generic)

instance ToJSON Issue

api :: Proxy API
api = Proxy

app :: Application
app = serve api issuesServer

issuesServer :: Server IssueAPI
issuesServer = getIssues :<|> showIssue
  where
    getIssues :: EitherT ServantErr IO [Issue]
    getIssues = return issues

    showIssue :: Int -> EitherT ServantErr IO Issue
    showIssue issueId' = let fi = Prelude.filter ((== issueId') . issueId) issues in
      case fi of
        [] -> left (err404 {errBody = "No issue with given id exists"})
        [issue] -> return issue
        _ -> left (err404 {errBody = "No issue with given id exists"})

issues = [Issue 1 "title1" "body1", Issue 2 "title2" "body2", Issue 3 "title3" "body3"]

main :: IO ()
main = run 8081 app
