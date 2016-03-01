module Main (..) where

import StartApp
import TransitRouter exposing (WithRoute, getTransition)
import Routes exposing (..)

type Route
  = Home
  | Hello
  | NotFound
  | EmptyRoute


routeParsers : List (Mathcer Route)
routeParsers =
  [ static Home "/"
  , static Hello "/hello"
  ]


type alias Model =
  WithRoute
  Routes.Route
  { foo: String }               
