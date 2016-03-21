module IssueList (..) where

import ServerApi exposing (..)
import Effects exposing (Effects, none)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (on, targetValue, onClick)
import Routes 
import Date
import Date.Format as DateFormat
import Issue exposing (..)
import Debug

type alias Model =
     { issues : List Issue }


type Action =
    Show
  | HandleIssuesRetrieved (Maybe (List Issue))

init : Model
init = Model [] 

update : Action -> Model -> (Model, Effects Action)
update action model =
   case action of
    Show ->
      ( model, getIssues HandleIssuesRetrieved )

    HandleIssuesRetrieved xs ->
      ( {model | issues = (Maybe.withDefault [] xs) }
      , Effects.none
      )



view : Signal.Address Action -> Model -> Html
view address model =
  div [class "container"]
      [ table [class "table table-striped table-hover"] [
          thead [] [
            tr [] [
               th [] [text "ID"]
              ,th [] [text "タイトル"]
              ,th [] [text "優先度"]
              ,th [] [text "締切"]
              ,th [] []
          ]
        ]
      , tbody [] (List.map issueRow model.issues)
    ]
  ]
  
issueRow : Issue -> Html
issueRow issue =                                     
  tr [] [
     td [] [text (toString issue.id)]
    ,td [] [text issue.title]
    ,td [] [priorityToHtml issue.priority]
    ,td [] [text (case issue.deadline of
                    Just date -> DateFormat.format "%Y/%m/%d" date
                    Nothing -> ""
                         )]
    ,td [] [button [ (Routes.clickAttr <| Routes.IssueDetailPage issue.id), class "btn btn-primary" ] [text "詳細"]]
  ]
