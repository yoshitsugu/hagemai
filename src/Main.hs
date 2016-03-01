{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeOperators         #-}


module Main where

import           Control.Monad.IO.Class     (liftIO)
import qualified Control.Monad.Trans.Either as E (EitherT, left)
import           Data.Aeson
import           Data.Text
import           Database.HDBC.Record       (runQuery)
import           Database.Relational.Query
import           DataSource                 (Connection, connect)
import           GHC.Generics
import qualified Issue
import           Network.Wai
import           Network.Wai.Handler.Warp   (run)
import           Servant
import           Servant.API

type IssueAPI =
  Get '[JSON] [Issue.Issue]
  :<|> Capture "issueId" Int :> Get '[JSON] Issue.Issue

type API = "issues" :> IssueAPI

api :: Proxy API
api = Proxy

app :: Connection -> Application
app conn = serve api (issuesServer conn)

issuesServer :: Connection -> Server IssueAPI
issuesServer conn = getIssues :<|> showIssue
  where
    getIssues :: E.EitherT ServantErr IO [Issue.Issue]
    getIssues = liftIO $ runQuery conn (relationalQuery issues) ()

    showIssue :: Int -> E.EitherT ServantErr IO Issue.Issue
    showIssue issueId = do
      r <- liftIO $ runQuery conn (relationalQuery (issue issueId)) ()
      case r of
        [] -> E.left (err404 {errBody = "No issue with given id exists"})
        [issue] -> return issue
        _ -> E.left (err404 {errBody = "No issue with given id exists"})

issues :: Relation () Issue.Issue
issues = relation $ query Issue.issue

issue :: Int -> Relation () Issue.Issue
issue id = relation $ do
  i <- query Issue.issue
  wheres $
    i ! Issue.id' .=. value (fromIntegral id)
  return i

main :: IO ()
main = do
  conn <- connect
  run 8081 $ app conn
