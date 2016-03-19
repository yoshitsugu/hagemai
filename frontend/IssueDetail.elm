module IssueDetail (..) where

import ServerApi exposing (..)
import Effects exposing (Effects, none)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (on, targetValue, onClick)
import Routes 
import Date
import Date.Format as DateFormat
import Issue exposing (..)
import Comment exposing (..)
import Util exposing (..)

type alias Model =
     { issue : Maybe Issue, comments : List Comment }

type Action
  = IssueDetailRetrieved (Maybe (Issue, List Comment))
  | Show Int

init : Model
init = Model Nothing []

update : Action -> Model -> (Model, Effects Action)
update action model =
  case action of
    IssueDetailRetrieved ics ->
      case ics of
        Just (i, cs) -> ( {model | issue = (Just i), comments = cs }, Effects.none )
        _ -> (model, Effects.none)

    Show i ->
      ( model, getIssueAndComments i IssueDetailRetrieved )

view : Signal.Address Action -> Model -> Html
view address model =
  case model.issue of
    Just is -> div [class "container"] [
                   h1 [] [priorityToHtml is.priority, text (" " ++ (toString is.id) ++ ":" ++ is.title)]
                 , div [class "well well-sm"]
                         [ i [class "fa fa-fw fa-calendar-o"] []
                         , text (DateFormat.format "%Y/%m/%d" is.deadline)
                         ]
                 , div [class "panel panel-default"] 
                     [ div [class "panel-heading"] [text (DateFormat.format "%Y/%m/%d" is.deadline)]
                     , div [class "panel-body"] (nl2br is.body)
                     ]
                 , div [class "comments"] (List.map commentPanel model.comments)
               ]
    Nothing -> text "No issue found"

commentPanel : Comment -> Html
commentPanel comment =
  div [class "panel panel-default"] [
       div [class "panel-heading"]
           [ i [class "fa fa-fw fa-envelope-o"] []
           , text (comment.email ++ " ")
           , i [class "fa fa-fw fa-clock-o"] []
           , text (DateFormat.format "%Y/%m/%d %H:%M" comment.createdAt)
           ]
     , div
       [class "panel-body"]
       (nl2br comment.body)
     ]  
