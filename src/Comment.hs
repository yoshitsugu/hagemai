{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}

module Comment where

import           Data.Aeson
import           DataSource (defineTable)


$(defineTable "comment")

instance ToJSON Comment

