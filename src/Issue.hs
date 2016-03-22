{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}

module Issue where

import           Data.Aeson
import           Data.Int            (Int32)
import           Data.Time.Calendar
import           Data.Time.LocalTime
import           DataSource          (defineTable)
import           GHC.Generics        (Generic)

$(defineTable "issue")

instance ToJSON Issue

data IssueId = IssueId { issId :: Integer } deriving (Show, Generic)
instance ToJSON IssueId

data IssueForm = IssueForm
  { ifTitle    :: String
  , ifEmail    :: String
  , ifBody     :: String
  , ifPriority :: Int32
  , ifDeadline :: Maybe Day
  } deriving (Show, Generic)

instance FromJSON IssueForm


issueFromForm :: LocalTime -> IssueForm -> Issue
issueFromForm ct issue = Issue 0 (ifEmail issue)  (ifTitle issue) (ifBody issue) (ifPriority issue) (ifDeadline issue) 1 ct ct

