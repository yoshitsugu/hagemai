module NewIssue (..) where

import Effects exposing (Effects, Never)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (on, targetValue, onClick)
import Issue exposing (..)
import Routes 
import ServerApi exposing (..)

type alias Model = { issueForm : IssueForm }

init : Model
init = Model {ifTitle = "", ifBody = ""}

type Action
  = PostIssue
  | SetIssueTitle (String)
  | SetIssueBody (String)
  | HandleSaved (Maybe Issue)
  | NoOp

update : Action -> Model -> (Model, Effects Action)
update action model =
  case action of
    NoOp ->
      ( model, Effects.none )

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
  div [class "container"] [
        h1 [] [text "Create Issue" ]
      , issueForm address model
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
          , value ""
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
        [ textarea
          [ class "form-control" 
          , value ""
          , rows 10
          , on "input" targetValue (\str -> Signal.message address (SetIssueBody str))
          ]
          []
        ]
      ]
    , div
      [ class "form-group"]
      [ label [ class "col-sm-2 control-label" ] [ text "Deadline" ]
      , div
        [ class "col-sm-10" ]
        [ input
          [ class "form-control" 
          , type' "date"
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
      
          
  


