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
import List exposing (indexedMap)
import String

type alias Model =
     { issue : Maybe Issue, comments : List Comment, newComment : CommentForm}

type Action
  = IssueDetailRetrieved (Maybe (Issue, List Comment))
  | Show Int
  | PostComment
  | SetCommentTitle (String)
  | SetCommentEmail (String)
  | SetCommentBody (String)
  | SetCommentPriority (String)
  | SetCommentDeadline (String)
  | HandleSaved (Maybe IssueId)
  | NoOp

init : Model
init = Model Nothing [] initCommentForm

update : Action -> Model -> (Model, Effects Action)
update action model =
  case action of
    IssueDetailRetrieved ics ->
      case ics of
        Just (i, cs) -> ( {model | issue = (Just i), comments = cs, newComment = Issue.commentFormFromIssue i}, Effects.none )
        _ -> (model, Effects.none)

    Show i ->
      ( model, getIssueAndComments i IssueDetailRetrieved )

    NoOp ->
      ( model, Effects.none )

    SetCommentTitle txt ->
      let nc = model.newComment
          nc' = {nc | cfTitle = txt}
      in
      ( {model | newComment = nc' }
      , Effects.none
      )

    SetCommentEmail txt ->
      let nc = model.newComment
          nc' = {nc | cfEmail = txt}
      in
      ( {model | newComment = nc'}
      , Effects.none
      )

    SetCommentBody txt ->
      let nc = model.newComment
          nc' = {nc | cfBody = txt}
      in
      ( {model | newComment = nc' }
      , Effects.none
      )

    SetCommentPriority txt ->
      let nc = model.newComment
          nc' = {nc | cfPriority = txt}
      in
      ( {model | newComment = nc' }
      , Effects.none
      )

    SetCommentDeadline txt ->
      let nc = model.newComment
          nc' = {nc | cfDeadline = if (String.length txt) > 0
           then (case Date.fromString txt of
             Ok d -> Just d
             Err err -> Nothing
           )
           else Nothing }
      in
      ( {model | newComment = nc' }
      , Effects.none
      )

    PostComment -> ( model, createComment model.newComment HandleSaved )

    HandleSaved id ->
      case id of
        Just id' ->
          ( model
          , Effects.map (\_ -> NoOp) (Routes.redirect (Routes.IssueDetailPage id'.issId))
          )
        Nothing -> (model,  Effects.map (\_ -> NoOp) (Routes.redirect Routes.IssueListPage))


view : Signal.Address Action -> Model -> Html
view address model =
  case model.issue of
    Just is -> div [class "container"] [
                   h1 [] [text ("#" ++ (toString is.id) ++ "  " ++ is.title)]
                 , h3 [class ""]
                         [ priorityToHtml is.priority
                         , i [class "fa fa-fw fa-calendar-o"] []
                         , text (case is.deadline of
                                   Just date -> DateFormat.format "%Y/%m/%d" date
                                   Nothing -> "")
                         ]
                 , hr [] []
                 , div [class "comments"] (List.map commentPanel (indexedMap (,) model.comments))
                 , br [] []
                 , h3 [] [text "Add Comment"]
                 , commentFormView address model
               ]
    Nothing -> text "No issue found"

commentPanel : (Int, Comment) -> Html
commentPanel (index, comment) =
  let ind = toString (index + 1) in
  div [class "panel panel-default", id ind] [
       div [class "panel-heading"]
           [ a [href ("#" ++ ind)] [text ind]
           , text " "
           , span [title "From"]
               [ i [class "fa fa-fw fa-envelope-o"] []
               , text (comment.email ++ " ")
               ]
           , span [title "Submitted At"]
               [ i [class "fa fa-fw fa-clock-o"] []
               , text (DateFormat.format "%Y/%m/%d %H:%M" comment.createdAt)
               ]
           ]
     , div
       [class "panel-body"]
       (nl2br comment.body)
     ]  

commentFormView : Signal.Address Action -> Model -> Html
commentFormView address model =
  let cf = model.newComment
  in
  Html.form
    [ class "form-horizontal"]
    [ div
      [ class "form-group" ]
      [ label [ class "col-sm-2 control-label" ] [ text "Email" ]
      , div
        [ class "col-sm-10" ]
        [ input
          [ class "form-control"
          , value cf.cfEmail
          , on "input" targetValue (\str -> Signal.message address (SetCommentEmail str))
          ]
          []
        ]
      ]
    , div
      [ class "form-group" ]
      [ label [ class "col-sm-2 control-label" ] [ text "Title" ]
      , div
        [ class "col-sm-10" ]
        [ input
          [ class "form-control"
          , value cf.cfTitle
          , on "input" targetValue (\str -> Signal.message address (SetCommentTitle str))
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
          , value cf.cfBody
          , rows 8
          , on "input" targetValue (\str -> Signal.message address (SetCommentBody str))
          ]
          []
        ]
      ]
    , div
      [ class "form-group"]
      [ label [ class "col-sm-2 control-label" ] [ text "Priority" ]
      , div
        [ class "col-sm-10"
        , on "change" targetValue (\str -> Signal.message address (SetCommentPriority str))
        ]
        [ select
          [ class "form-control" ]
          [ option [value "1", selected (cf.cfPriority == "1")] [text "緊急"]
          , option [value "2", selected (cf.cfPriority == "2")] [text "高"]
          , option [value "3", selected (cf.cfPriority == "3")] [text "中"]
          , option [value "4", selected (cf.cfPriority == "4")] [text "低"]
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
          , value (case cf.cfDeadline of
                     Just deadline -> (DateFormat.format "%Y-%m-%d" deadline)
                     Nothing -> "")
          , on "change" targetValue (\str -> Signal.message address (SetCommentDeadline str))
          ]
          []
        ]
      ]
    , div
      [ class "form-group"]
      [ div
         [ class "col-sm-10 col-sm-offset-2"
         , onClick address PostComment
         ]
         [ button [ class "btn btn-primary btn-block" ] [ text "Submit" ] ]
      ]
    ]
