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
                    

createIssue : IssueForm -> (Maybe Int -> b) -> Effects.Effects b
createIssue issue action =              
  Http.send Http.defaultSettings
     { verb = "POST"
     , url = baseUrl ++ "/issues"
     , body = Http.string (Debug.log "ei" (encodeIssue issue))
     , headers = [("Content-Type", "application/json")]
    }
  |> Http.fromJson Json.int
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
        , ("ifPriority", case String.toInt a.ifPriority of
                           Ok i -> JsonE.int i
                           Err e -> JsonE.null
          )
        , ("ifDeadline", case a.ifDeadline of
                           Just date -> (JsonE.string (DateFormat.format "%Y-%m-%d" date))
                           Nothing -> JsonE.null)
                                      
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
