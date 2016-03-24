{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeOperators         #-}

module Main where

import qualified Comment
import           Control.Monad.IO.Class         (liftIO)
import qualified Control.Monad.Trans.Either     as E (EitherT, left)
import           Data.Aeson
import           Data.Text
import           Data.Time.Clock                (getCurrentTime)
import           Data.Time.LocalTime
import           Database.HDBC                  (withTransaction)
import           Database.HDBC.Record           (runInsert, runQuery, runUpdate)
import           Database.Relational.Query
import qualified Database.Relational.Query      as Q ((!))
import           Database.Relational.Query.Type (Update (..), unsafeTypedQuery)
import           DataSource                     (Connection, connect)
import           Debug.Trace
import           GHC.Generics
import           IndexPage
import qualified Issue
import           Network.Wai
import           Network.Wai.Handler.Warp       (run)
import           Servant
import           Servant.API
import           Servant.HTML.Blaze             (HTML)
import           System.Directory               (getCurrentDirectory)

type IssueAPI =
  Get '[JSON] [Issue.Issue]
  :<|> Capture "issueId" Integer :> Get '[JSON] (Issue.Issue, [Comment.Comment])
  :<|> ReqBody '[JSON] Issue.IssueForm :> Post '[JSON] Issue.IssueId
  :<|> Capture "issueId" Integer :> "comments" :> ReqBody '[JSON] Comment.CommentForm :> Post '[JSON] Issue.IssueId

type API =
  "assets" :> Raw
  :<|> "api" :> "issues" :> IssueAPI
  :<|> Get '[HTML] IndexPage
  :<|> "issues" :> "new" :> Get '[HTML] IndexPage
  :<|> "issues" :> Capture "issueId" Integer :> Get '[HTML] IndexPage

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
issuesServer conn = getIssues :<|> showIssue :<|> createIssue :<|> createComment
  where
    getCurrentLocalTime :: E.EitherT ServantErr IO LocalTime
    getCurrentLocalTime = do
      let tz = hoursToTimeZone 9
      ctu <- liftIO getCurrentTime
      return $ utcToLocalTime tz ctu


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

    createIssue :: Issue.IssueForm -> E.EitherT ServantErr IO Issue.IssueId
    createIssue issue = do
      ct <- getCurrentLocalTime
      let is = Issue.issueFromForm ct issue
      iid <- liftIO $ withTransaction conn
        (\c -> do
          cmi <- runInsert c Issue.insertIssue is
          q <- runQuery c (unsafeTypedQuery "select LAST_INSERT_ID()") ()  :: IO [Integer]
          case q of
            [] -> return 0
            [i] -> return i
            _ -> return 0
        )
      return $ Issue.IssueId iid


    createComment :: Integer -> Comment.CommentForm -> E.EitherT ServantErr IO Issue.IssueId
    createComment issueId comment = do
      ct <- getCurrentLocalTime
      let cm = Comment.commentFromForm ct comment
      _ <- liftIO $ withTransaction conn (\c -> runInsert c Comment.insertComment cm)
      q <- liftIO (runQuery conn (unsafeTypedQuery "select LAST_INSERT_ID()") ()  :: IO [Integer])
      case q of
        [] -> return 0
        [i] -> liftIO $ withTransaction conn (\c -> liftIO $! runUpdate c (updateIssueByComment cm ct) ())
        _ -> return 0
      return . Issue.IssueId . fromIntegral $ Comment.issueId cm


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

updateIssueByComment :: Comment.Comment -> LocalTime -> Update ()
updateIssueByComment cm ct = typedUpdate Issue.tableOfIssue . updateTarget $ \proj -> do
  Issue.title' <-# value (Comment.title cm)
  Issue.deadline' <-# value (Comment.deadline cm)
  Issue.priority' <-# value (Comment.priority cm)
  Issue.state' <-# value (Comment.state cm)
  Issue.updatedAt' <-# value ct
  wheres $ proj Q.! Issue.id' .=. value (Comment.issueId cm)

main :: IO ()
main = do
  conn <- connect
  curDir <- getCurrentDirectory
  run 8081 $ app conn curDir
