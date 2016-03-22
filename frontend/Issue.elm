module Issue where

import Date
import Html exposing (..)
import Html.Attributes exposing (..)


type alias Issue =
  { id: Int
  , email: String
  , title: String
  , body: String
  , priority: Int
  , state: Int
  , deadline: Maybe Date.Date
  , createdAt: Date.Date
  , updatedAAt: Date.Date
  }

type alias IssueForm =
  { ifTitle : String
  , ifEmail : String
  , ifBody : String
  , ifPriority : String
  , ifDeadline : Maybe Date.Date               
  }

type alias CommentForm =
  { cfTitle : String
  , cfEmail : String
  , cfBody : String
  , cfPriority : String
  , cfState : Int
  , cfDeadline : Maybe Date.Date
  , cfIssueId : Int             
  }

type alias IssueId = { issId : Int }                     

priorityToHtml : Int -> Html
priorityToHtml i
  = case i of
      1 -> div [class "label label-danger"] [text "緊急"]
      2 -> div [class "label label-warning"] [text "高"]
      3 -> div [class "label label-info"] [text "中"]
      4 -> div [class "label label-default"] [text "低"]
      _ -> text ""

commentFormFromIssue : Issue -> CommentForm
commentFormFromIssue is =
  { cfTitle = is.title
  , cfEmail = ""
  , cfBody = ""
  , cfPriority = toString is.priority
  , cfState = is.state
  , cfDeadline = is.deadline
  , cfIssueId = is.id
  }

initCommentForm : CommentForm
initCommentForm =
  { cfTitle = ""
  , cfEmail = ""
  , cfBody = ""
  , cfPriority = "3"
  , cfState = 1
  , cfDeadline = Nothing
  , cfIssueId = 0
  }    
