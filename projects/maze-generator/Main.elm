module Main exposing (main)

import AnimationFrame
import Html exposing (Html)
import Html.App exposing (program)
import Random.Pcg
import Time exposing (Time)

main : Program Never
main =
  program
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }


-- MODEL

-- let's first do the version where the maze is from a 2D grid and walls are the borders of the squares
-- going to do a full random DFS search  

type alias Model = Time


init : (Model, Cmd Msg)
init =
  (0, Cmd.none)


-- UPDATE

type Msg
  = Tick Time


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Tick newTime ->
      (newTime, Cmd.none)


-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
  AnimationFrame.diffs Tick


-- VIEW

view : Model -> Html Msg
view model =
  Html.text "Hello world"
