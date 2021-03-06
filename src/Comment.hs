{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}

module Comment where

import           Data.Aeson
import           Data.Int            (Int32)
import           Data.Time.Calendar
import           Data.Time.LocalTime
import           DataSource          (defineTable)
import           GHC.Generics        (Generic)
import qualified Issue


$(defineTable "comment")

instance ToJSON Comment

data CommentForm = CommentForm
  { cfTitle    :: String
  , cfEmail    :: String
  , cfBody     :: String
  , cfPriority :: Int32
  , cfState    :: Int32
  , cfDeadline :: Maybe Day
  , cfIssueId  :: Int32
 } deriving (Show, Generic)

instance FromJSON CommentForm

commentFromForm :: LocalTime -> CommentForm -> Comment
commentFromForm ct cf = Comment 0 (cfIssueId cf) (cfEmail cf) (cfTitle cf) (cfBody cf) (cfPriority cf) (cfDeadline cf) (cfState cf) ct ct

type IssueId = Integer

fromIssue :: Issue.Issue -> IssueId -> Comment
fromIssue issue isId = Comment
  { issueId = fromIntegral isId
  , title = Issue.title issue
  , body = Issue.body issue
  , email = Issue.email issue
  , priority = Issue.priority issue
  , state = Issue.state issue
  , deadline = Issue.deadline issue
  , updatedAt = Issue.updatedAt issue
  , createdAt = Issue.createdAt issue
  , Comment.id = 0
  }
