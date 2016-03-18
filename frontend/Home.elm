module Home (..) where

import Effects exposing (Effects, Never)
import Html exposing (..)


type alias Model = String

init : Model
init = "hoge"

view : Signal.Address Action -> Model -> Html
view address model =
  text ("hello" ++ model)

type alias Action = String -> String

update : Action -> Model -> (Model, Effects Action)
update action model = ("", Effects.none)


