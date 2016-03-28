module NewIssue (..) where

import Effects exposing (Effects, Never)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (on, targetValue, onClick, onWithOptions)
import Issue exposing (..)
import Routes 
import ServerApi exposing (..)
import Date
import Date.Format as DateFormat
import Debug
import String
import Json.Decode as Json

type alias Model = IssueForm

init : Model
init = {ifTitle = "", ifEmail = "", ifBody = "", ifPriority = "3", ifDeadline = Nothing}

type Action
  = PostIssue
  | SetIssueTitle (String)
  | SetIssueEmail (String)
  | SetIssueBody (String)
  | SetIssuePriority (String)
  | SetIssueDeadline (String)
  | HandleSaved (Maybe IssueId)
  | NoOp

update : Action -> Model -> (Model, Effects Action)
update action model =
  case action of
    NoOp ->
      ( model, Effects.none )

    SetIssueTitle txt ->
      ( {model | ifTitle = txt}
      , Effects.none
      )

    SetIssueEmail txt ->
      ( {model | ifEmail = txt}
      , Effects.none
      )

    SetIssueBody txt ->
      ( {model | ifBody = txt }
      , Effects.none
      )

    SetIssuePriority txt ->
      ( {model | ifPriority = txt }
      , Effects.none
      )

    SetIssueDeadline txt -> 
      ( {model | ifDeadline = if (String.length txt) > 0
           then (case Date.fromString txt of
             Ok d -> Just d
             Err err -> Nothing
           )
           else Nothing }
      ,  Effects.none
      )

    PostIssue -> ( model, createIssue model HandleSaved )

    HandleSaved maybeId ->
      case maybeId of
        Just justId ->
          ( model
          , Effects.map (\_ -> NoOp) (Routes.redirect (Routes.IssueDetailPage justId.issId))
          )
        Nothing -> Debug.crash "Save failed... we're not handling it..."


view : Signal.Address Action -> Model -> Html
view address model =
  div [class "container"] [
        h1 [] [text "New Issue" ]
      , issueForm address model
      ]

issueForm : Signal.Address Action -> Model -> Html
issueForm address model =
  Html.form
    [ class "form-horizontal"]
    [ div
      [ class "form-group" ]
      [ label [ class "col-sm-2 control-label" ] [ text "Email" ]
      , div
        [ class "col-sm-10" ]
        [ input
          [ class "form-control"
          , value model.ifEmail
          , on "input" targetValue (\str -> Signal.message address (SetIssueEmail str))
          ]
          []
        ]
      ]
    , div
      [ class "form-group" ]
      [ label [ class "col-sm-2 control-label" ] [ text "Titile" ]
      , div
        [ class "col-sm-10" ]
        [ input
          [ class "form-control"
          , value model.ifTitle
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
          , value model.ifBody
          , rows 8
          , on "input" targetValue (\str -> Signal.message address (SetIssueBody str))
          ]
          []
        ]
      ]
    , div
      [ class "form-group"]
      [ label [ class "col-sm-2 control-label" ] [ text "Priority" ]
      , div
        [ class "col-sm-10"
        , on "change" targetValue (\str -> Signal.message address (SetIssuePriority str))
        ]
        [ select
          [ class "form-control" ]
          [ option [value "1", selected (model.ifPriority == "1")] [text "緊急"]
          , option [value "2", selected (model.ifPriority == "2")] [text "高"]
          , option [value "3", selected (model.ifPriority == "3")] [text "中"]
          , option [value "4", selected (model.ifPriority == "4")] [text "低"]
          ]
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
          , value (case model.ifDeadline of
                     Just deadline -> (DateFormat.format "%Y-%m-%d" deadline)
                     Nothing -> "")
          , on "change" targetValue (\str -> Signal.message address (SetIssueDeadline str))
          ]
          []
        ]
      ]
    , div
      [ class "form-group"]
      [ div
         [ class "col-sm-10 col-sm-offset-2"]
         [ button
            [ class "btn btn-block btn-primary"
            , onWithOptions
              "click"
              { stopPropagation = True, preventDefault = True }
              Json.value
              (\_ -> Signal.message address (Debug.log "submit" PostIssue))
            ]
            [ text "Save" ]
         ]
      ]
    ]
      
          
  


