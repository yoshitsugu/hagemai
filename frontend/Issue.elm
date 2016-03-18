module Issue where
import Date



type alias Issue =
  { id: Int
  , title: String
  , body: String
  , priority: Int
  , deadline: Date.Date
  }

type alias IssueForm =
  { ifTitle : String
  , ifBody : String
  }

priorityToString : Int -> String
priorityToString i
  = case i of
      1 -> "緊急"
      2 -> "高"
      3 -> "中"
      4 -> "低"
      _ -> ""

