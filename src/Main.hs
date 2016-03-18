{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeOperators         #-}

module Main where

import qualified Comment
import           Control.Monad.IO.Class     (liftIO)
import qualified Control.Monad.Trans.Either as E (EitherT, left)
import           Data.Aeson
import           Data.Text
import           Data.Time.Clock            (getCurrentTime)
import           Data.Time.LocalTime
import           Database.HDBC              (withTransaction)
import           Database.HDBC.Record       (runInsert, runQuery)
import           Database.Relational.Query
import qualified Database.Relational.Query  as Q ((!))
import           DataSource                 (Connection, connect)
import           GHC.Generics
import           IndexPage
import qualified Issue
import           Network.Wai
import           Network.Wai.Handler.Warp   (run)
import           Servant
import           Servant.API
import           Servant.HTML.Blaze         (HTML)
import           System.Directory           (getCurrentDirectory)

type IssueAPI =
  Get '[JSON] [Issue.Issue]
  :<|> Capture "issueId" Integer :> Get '[JSON] (Issue.Issue, [Comment.Comment])
  :<|> ReqBody '[JSON] Issue.IssueForm :> Post '[JSON] Issue.Issue

type API =
  "assets" :> Raw
  :<|> "api" :> "issues" :> IssueAPI
  :<|> Get '[HTML] IndexPage
  :<|> "is" :> Get '[HTML] IndexPage
  :<|> "is" :> Capture "issueId" Integer :> Get '[HTML] IndexPage

api :: Proxy API
api = Proxy

app :: Connection -> FilePath -> Application
app conn curDir = serve api $ assetsServer curDir
                            :<|> issuesServer conn
                            :<|> return IndexPage
                            :<|> return IndexPage
                            :<|> (\_ -> return IndexPage)

assetsServer :: FilePath -> Server Raw
assetsServer curDir = serveDirectory (curDir ++  "/static/")

issuesServer :: Connection -> Server IssueAPI
issuesServer conn = getIssues :<|> showIssue :<|> createIssue
  where
    getIssues :: E.EitherT ServantErr IO [Issue.Issue]
    getIssues = liftIO $ runQuery conn (relationalQuery issues) ()

    showIssue :: Integer -> E.EitherT ServantErr IO (Issue.Issue, [Comment.Comment])
    showIssue issueId = do
      r <- liftIO $ runQuery conn (relationalQuery (issueById issueId)) ()
      case r of
        [] -> E.left (err404 {errBody = "No issue with given id exists"})
        [is] -> do
          cs <- liftIO $ runQuery conn (relationalQuery (commentsByIssueId issueId)) ()
          return (is, cs)
        _ -> E.left (err404 {errBody = "No issue with given id exists"})

    createIssue :: Issue.IssueForm -> E.EitherT ServantErr IO Issue.Issue
    createIssue issue = do
      let tz = hoursToTimeZone 9
      ctu <- liftIO getCurrentTime
      let ct = utcToLocalTime tz ctu
          is = toIssue issue ct
      insertedId <- liftIO $ withTransaction conn (\c -> runInsert c Issue.insertIssue is)
      r <- liftIO $ runQuery conn (relationalQuery (issueById insertedId)) ()
      case r of
        [] -> E.left (err404 {errBody = "No issue with given id exists"})
        [i] -> return i
        _ -> E.left (err404 {errBody = "No issue with given id exists"})

      where
        toIssue issue ct = Issue.Issue 0 "hoge@example.com" (Issue.ifTitle issue) (Issue.ifBody issue) 1 (Just (localDay ct)) 1 ct ct

issues :: Relation () Issue.Issue
issues = relation $ query Issue.issue

issueById :: Integer -> Relation () Issue.Issue
issueById id = relation $ do
  i <- query Issue.issue
  wheres $
    i Q.! Issue.id' .=. value (fromIntegral id)
  pure i

commentsByIssueId :: Integer -> Relation () Comment.Comment
commentsByIssueId id = relation $ do
  c <- query Comment.comment
  wheres $
    c Q.! Comment.issueId' .=. value (fromIntegral id)
  asc $ c Q.! Comment.createdAt'
  pure c


main :: IO ()
main = do
  conn <- connect
  curDir <- getCurrentDirectory
  run 8081 $ app conn curDir
