module Util where

import Html exposing (..)
import Html.Attributes exposing (..)
import List exposing (..)
import Regex
import Debug

nl2br : String -> List Html
nl2br str = Regex.split (Regex.All) (Regex.regex "\r?\n") str
            |> Debug.log "kore"
            |> map text
            |> intersperse (br [] [])
