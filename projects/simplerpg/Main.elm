module Main exposing (main)

import Html exposing (Html)
import Svg exposing (Svg, Attribute)
import Svg.Attributes as SAttr
import Time exposing (Time)
import Task
import Color exposing (Color)
import Math.Vector2 exposing (..)
import String
import AnimationFrame
import Random.Pcg as Random exposing (Generator, Seed)


main : Program Never Model Msg
main =
  Html.program
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }

type alias FPSCounter = { fps : Float }

newFPSCounter : FPSCounter
newFPSCounter = {fps=0.0}

getFPS : FPSCounter -> Float
getFPS = .fps

-- use exponential decay, but weighted with longer incoming diffs
updateFPSCounter : FPSCounter -> Time -> FPSCounter
updateFPSCounter {fps} diff =
    let
        s = Time.inSeconds diff + 0.0000001 -- avoid division by zero
        w = 1.0 / (1.0 + 10*e^(-s))
    in
    {fps = (1.0-w)*fps + w * (1.0 / s)}

type alias Model = 
    { fpsCounter : FPSCounter
    }

newModel : Model
newModel =
    { fpsCounter = newFPSCounter
    }

initModel : Model
initModel = newModel

init : (Model, Cmd Msg)
init =
  ( initModel
  , Cmd.none
  )


-- UPDATE

type Msg
  = Step Time

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Step diff ->
      ({model |
        fpsCounter = updateFPSCounter model.fpsCounter diff},
      Cmd.none)


-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
  AnimationFrame.diffs (Step << clamp 0.0 100.0)


-- VIEW

viewFPS : FPSCounter -> Html Msg
viewFPS counter =
    let 
        fps = getFPS counter
        n = round (fps * 100)
        a = toString (n // 100)
        b = String.padLeft 2 '0' <| toString (n % 100) 
    in
    Html.text <| "FPS: " ++ a ++ "." ++ b 

view : Model -> Html Msg
view model =
    let
        fps = viewFPS model.fpsCounter
    in
    Html.body []
    [ Html.p [] [fps]
    ] 
