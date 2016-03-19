module ServerApi where

import Html exposing (..)
import Html.Attributes exposing (..)
import Json.Decode as Json exposing ((:=))
import Json.Encode as JsonE
import Task exposing (..)
import Effects exposing (Effects, Never)
import Http
import Date
import Json.Decode.Extra as JsonEx
import Result exposing (toMaybe)
import Issue exposing (..)
import Comment exposing (..)
                 
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


issueDecoder : Json.Decoder Issue
issueDecoder =                                               
  Json.object8 Issue
    ("id" := Json.int)
    ("email" := Json.string)
    ("title" := Json.string)
    ("body" := Json.string)
    ("priority" := Json.int)
    ("deadline" := JsonEx.date )
    ("createdAt" := JsonEx.date )
    ("updatedAt" := JsonEx.date )
                    

createIssue : IssueForm -> (Maybe Issue -> b) -> Effects.Effects b
createIssue issue action =              
  Http.send Http.defaultSettings
     { verb = "POST"
     , url = baseUrl ++ "/issues"
     , body = Http.string (encodeIssue issue)
     , headers = [("Content-Type", "application/json")]
    }
  |> Http.fromJson issueDecoder
  |> Task.toMaybe
  |> Task.map action
  |> Effects.task

encodeIssue : IssueForm -> String
encodeIssue a =
  JsonE.encode 0 <|
    JsonE.object
      [
        ("ifTitle", JsonE.string a.ifTitle)
        , ("ifBody", JsonE.string a.ifBody)
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
