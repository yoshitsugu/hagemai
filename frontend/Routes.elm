module Routes  where

import Html exposing (Attribute)
import Html.Events exposing (on, onClick, onWithOptions)
import Html.Attributes exposing (href)
import Json.Decode as Json
import Signal
import Effects exposing (Effects, Never)
import TransitRouter
import RouteParser exposing (..)
import Debug exposing (log)

type Route
  = NewIssuePage
  | IssueListPage
  | IssueDetailPage Int
  | EmptyRoute

routeParsers : List (Matcher Route)
routeParsers =
  [ static NewIssuePage "/new"
  , static IssueListPage "/"
  , dyn1 IssueDetailPage "/is/" int ""
  ]

decode : String -> Route
decode path =
  RouteParser.match routeParsers path
   |> Maybe.withDefault EmptyRoute

encode : Route -> String
encode route =
  case route of
    NewIssuePage -> "/new"
    IssueListPage   -> "/"
    IssueDetailPage i -> "/is/" ++ toString i
    EmptyRoute -> ""

redirect : Route -> Effects ()
redirect route =                                       
  encode route
    |> Signal.send TransitRouter.pushPathAddress
    |> Effects.task


clickAttr : Route -> Attribute
clickAttr route =                                     
  on "click" Json.value (\_ ->  Signal.message TransitRouter.pushPathAddress <| encode route)

linkAttrs : Route -> List Attribute
linkAttrs route =                                     
  let
    path = encode route
  in
    [ href path
    , onWithOptions
        "click"
        { stopPropagation = True, preventDefault = True }
        Json.value
        (\_ ->  Signal.message TransitRouter.pushPathAddress (log "string" path))
    ]
