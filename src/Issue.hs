{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}

module Issue where

import           Data.Aeson
import           DataSource (defineTable)

$(defineTable "issue")

instance ToJSON Issue
