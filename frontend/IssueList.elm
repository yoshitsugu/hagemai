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

type alias Model =
     { issues : List Issue, issueForm : IssueForm }


type Action =
    Show
  | HandleIssuesRetrieved (Maybe (List Issue))
  | SetIssueTitle (String)
  | SetIssueBody (String)
  | PostIssue
  | NoOp
  | HandleSaved (Maybe Issue)

init : Model
init = Model [] {ifTitle = "", ifBody = ""}

update : Action -> Model -> (Model, Effects Action)
update action model =
   case action of
    NoOp ->
      ( model, Effects.none )

    Show ->
      ( model, getIssues HandleIssuesRetrieved )

    HandleIssuesRetrieved xs ->
      ( {model | issues = (Maybe.withDefault [] xs) }
      , Effects.none
      )

    SetIssueTitle txt ->
      ( {model | issueForm = {ifTitle = txt, ifBody = model.issueForm.ifBody}}
      , Effects.none
      )

    SetIssueBody txt ->
      ( {model | issueForm = {ifBody = txt, ifTitle = model.issueForm.ifTitle }}
      , Effects.none
      )

    PostIssue -> ( model, createIssue { ifTitle = model.issueForm.ifTitle, ifBody = model.issueForm.ifBody } HandleSaved )

    HandleSaved maybeIssue ->
      case maybeIssue of
        Just issue ->
          ( { model | issueForm = {
                ifTitle = issue.title
              , ifBody = issue.body
            }}
          , Effects.map (\_ -> NoOp) (Routes.redirect Routes.IssueListPage)
          )
        Nothing -> (model,  Effects.map (\_ -> NoOp) (Routes.redirect Routes.IssueListPage))




view : Signal.Address Action -> Model -> Html
view address model =
  div [class "container-fluid"] [
        h1 [] [text "Create Issue" ]
      , issueForm address model
      , h1 [] [text "Issues" ]
      , table [class "table table-striped"] [
          thead [] [
            tr [] [
               th [] [text "Id"]
              ,th [] [text "Name"]
              ,th [] [text "Priority"]
              ,th [] [text "deadline"]
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
    ,td [] [text (priorityToString issue.priority)]
    ,td [] [text (DateFormat.format "%Y/%m/%d" issue.deadline)]
    ,td [] [button [ (Routes.clickAttr <| Routes.IssueDetailPage issue.id), class "btn btn-default" ] [text "Detail"]]
  ]

issueForm : Signal.Address Action -> Model -> Html
issueForm address model =
  Html.form
    [ class "form-horizontal"]
    [ div
      [ class "form-group" ]
      [ label [ class "col-sm-2 control-label" ] [ text "Title" ]
      , div
        [ class "col-sm-10" ]
        [ input
          [ class "form-control"
          , value model.issueForm.ifTitle
          , on "input" targetValue (\str -> Signal.message address (SetIssueTitle str))
          ]
          []
        ]
      ]
    , div
      [ class "form-group"]
      [ label [ class "col-sm-2 control-label" ] [ text "Body" ]
      , div
        [ class "col-sm-10" ]
        [ input
          [ class "form-control" 
          , value model.issueForm.ifBody
          , on "input" targetValue (\str -> Signal.message address (SetIssueBody str))
          ]
          []
        ]
      ]
    , div
      [ class "form-group"]
      [ div
         [ class "col-sm-10 col-sm-offset-2"
         , onClick address PostIssue
         ]
         [ button [ class "btn btn-primary btn-block" ] [ text "Submit" ] ]
      ]
    ]
      
          
  

