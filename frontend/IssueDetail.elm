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
    Just is -> div [class "container-fluid"] [
                   div [class "panel panel-default"] 
                       [ div [class "panel-heading"] [text is.title]
                       , div [class "panel-body"] [ text is.body ]
                       ]
                 , div [class "comments"] (List.map commentPanel model.comments)
               ]
    Nothing -> text "No issue found"

commentPanel : Comment -> Html
commentPanel comment =
  div [class "panel panel-default"] [
       div [class "panel-heading"] [text (comment.email ++ ":" ++ (DateFormat.format "%Y/%m/%d %H:%M" comment.createdAt))]
     , div
       [class "panel-body"]
       [text comment.body]
     ]  
