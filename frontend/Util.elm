module Util where

import Html exposing (..)
import Html.Attributes exposing (..)
import List exposing (..)
import Regex

nl2br : String -> List Html
nl2br str = Regex.split (Regex.All) (Regex.regex "\r?\n") str
            |> map text
            |> intersperse (br [] [])
