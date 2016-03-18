module Comment where

import Date


type alias Comment =
  { email: String
  , body: String
  , createdAt: Date.Date
  }

