module Clock exposing (main)

import Html exposing (Html)
import Html.App as App
import Time exposing (Time, second)
import Date exposing (Date)
import Task
import String


main =
  App.program
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }


-- MODEL

type alias Model = Time


init : (Model, Cmd Msg)
init =
  (0, Task.perform identity Tick <| Time.now)


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
  Time.every second Tick


-- VIEW

view : Model -> Html Msg
view model =
    let
        d = Date.fromTime model
        h = Date.hour d
        m = Date.minute d
        s = Date.second d
    in
  Html.text <| String.join ":" <| List.map toString [h,m,s]
