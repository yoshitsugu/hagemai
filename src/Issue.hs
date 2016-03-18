{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}

module Issue where

import           Data.Aeson
import           Data.Int     (Int32)
import           DataSource   (defineTable)
import           GHC.Generics (Generic)


$(defineTable "issue")

instance ToJSON Issue

data IssueForm = IssueForm
  { ifTitle :: String
  , ifBody  :: String
  } deriving (Show, Generic)

instance FromJSON IssueForm

