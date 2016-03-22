module ServerApi where

import Html exposing (..)
import Html.Attributes exposing (..)
import Json.Decode as Json exposing ((:=))
import Json.Encode as JsonE
import Task exposing (..)
import Effects exposing (Effects, Never)
import Http
import Date
import Date.Format as DateFormat
import Json.Decode.Extra as JsonEx
import Result exposing (toMaybe)
import Issue exposing (..)
import Comment exposing (..)
import String
import Debug
                 
baseUrl : String
baseUrl = "http://localhost:8081/api"

getIssues : (Maybe (List Issue) -> a) -> Effects.Effects a
getIssues action =                                           
  Http.get issues (baseUrl ++ "/issues")
    |> Task.toMaybe
    |> Task.map action
    |> Effects.task



issues : Json.Decoder (List Issue)
issues =                                              
  Json.list issueDecoder

constructing : a -> Json.Decoder a
constructing = Json.succeed

apply : Json.Decoder (a -> b) -> Json.Decoder a -> Json.Decoder b
apply = Json.object2 (<|)

issueDecoder : Json.Decoder Issue
issueDecoder =
  constructing Issue
    `apply` ("id" := Json.int)
    `apply` ("email" := Json.string)
    `apply` ("title" := Json.string)
    `apply` ("body" := Json.string)
    `apply` ("priority" := Json.int)
    `apply` ("state" := Json.int)
    `apply` ("deadline" := Json.maybe JsonEx.date )
    `apply` ("createdAt" := JsonEx.date )
    `apply` ("updatedAt" := JsonEx.date )
                    

createIssue : IssueForm -> (Maybe IssueId -> b) -> Effects.Effects b
createIssue issue action =
  Http.send Http.defaultSettings
     { verb = "POST"
     , url = baseUrl ++ "/issues"
     , body = Http.string (encodeIssue issue)
     , headers = [("Content-Type", "application/json")]
    }
  |> Http.fromJson issueIdDecoder
  |> Debug.log "int" 
  |> Task.toMaybe
  |> Task.map action
  |> Effects.task

issueIdDecoder : Json.Decoder IssueId
issueIdDecoder = Json.object1 IssueId ("issId" := Json.int)
                 
encodeIssue : IssueForm -> String
encodeIssue a =
  JsonE.encode 0 <|
    JsonE.object
      [ ("ifTitle", JsonE.string a.ifTitle)
      , ("ifEmail", JsonE.string a.ifEmail)
      , ("ifBody", JsonE.string a.ifBody)
      , ("ifPriority", case String.toInt a.ifPriority of
                         Ok i -> JsonE.int i
                         Err e -> JsonE.null
        )
      , ("ifDeadline", case a.ifDeadline of
                         Just date -> (JsonE.string (DateFormat.format "%Y-%m-%d" date))
                         Nothing -> JsonE.null)
      ]


encodeComment : CommentForm -> String
encodeComment a =
  JsonE.encode 0 <|
    JsonE.object
      [ ("cfTitle", JsonE.string a.cfTitle)
      , ("cfEmail", JsonE.string a.cfEmail)
      , ("cfBody", JsonE.string a.cfBody)
      , ("cfState", JsonE.int a.cfState)
      , ("cfPriority", case String.toInt a.cfPriority of
                         Ok i -> JsonE.int i
                         Err e -> JsonE.null
        )
      , ("cfDeadline", case a.cfDeadline of
                         Just date -> (JsonE.string (DateFormat.format "%Y-%m-%d" date))
                         Nothing -> JsonE.null)
      , ("cfIssueId", JsonE.int a.cfIssueId)
      ]
    
getIssueAndComments : Int -> (Maybe (Issue, (List Comment)) -> a) -> Effects.Effects a
getIssueAndComments issueId action =
  Http.get issueAndComment (baseUrl ++ "/issues/" ++ (toString issueId))
    |> Task.toMaybe
    |> Task.map action
    |> Effects.task


commentDecoder : Json.Decoder Comment
commentDecoder = 
  Json.object3 Comment
      ("email" := Json.string)
      ("body" := Json.string)
      ("createdAt" := JsonEx.date)

issueAndComment : Json.Decoder (Issue, (List Comment))
issueAndComment =
  Json.tuple2 (,) issueDecoder (Json.list commentDecoder)


createComment: CommentForm -> (Maybe IssueId -> b) -> Effects.Effects b
createComment commentForm action =
  Http.send Http.defaultSettings
     { verb = "POST"
     , url = baseUrl ++ "/issues/" ++ (toString commentForm.cfIssueId) ++ "/comments"
     , body = Http.string (encodeComment commentForm)
     , headers = [("Content-Type", "application/json")]
    }
  |> Http.fromJson issueIdDecoder
  |> Debug.log "int" 
  |> Task.toMaybe
  |> Task.map action
  |> Effects.task

