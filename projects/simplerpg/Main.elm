module Main exposing (main)

import Html exposing (Html)
import Svg exposing (Svg, Attribute)
import Svg.Attributes as SA
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


-- Global parameters

-- maximum and minimum milliseconds that are allowed in-between frames
-- any value outside is clamped
maxFrameDiff : Float
maxFrameDiff = 100.0

minFrameDiff : Float
minFrameDiff = 1e-4

type alias FPSCounter = { frameTime : Float }

newFPSCounter : FPSCounter
newFPSCounter = {frameTime=1000.0/60.0}

getFPS : FPSCounter -> Float
getFPS {frameTime} = 1000.0 / frameTime

-- use simple exponential decay
updateFPSCounter : FPSCounter -> Time -> FPSCounter
updateFPSCounter ({frameTime} as counter) diff =
    let
        decay = 0.97
        frameTime2 = frameTime * decay + (1.0 - decay) * diff
    in
    {counter | frameTime = frameTime2}

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
  AnimationFrame.diffs <| Step << clamp minFrameDiff maxFrameDiff


-- VIEW

viewFPS : FPSCounter -> Html Msg
viewFPS counter =
    let 
        fps = getFPS counter
        n = floor (fps * 10)
        a = toString (n // 10)
        b = String.padLeft 1 '0' <| toString (n % 10) 
    in
    Html.text <| "FPS: " ++ a ++ "." ++ b 

viewSvgTest : Html Msg
viewSvgTest = 
    let
      width = 600
      height = 600
      wStr = toString width
      hStr = toString height
      viewStr = "0 0 " ++ wStr ++ " " ++ hStr
      rect1 = Svg.rect [SA.x "0", SA.y "0", SA.width "100", SA.height "100", SA.fill "white"] []
      rect2 = Svg.rect [SA.x "200", SA.y "300", SA.width "100", SA.height "100", SA.fill "red"] []
      bg = Svg.rect [SA.x "0", SA.y "0", SA.width wStr, SA.height hStr, SA.fill "black"]  []
    in
      Svg.svg [SA.width wStr, SA.height hStr, SA.viewBox viewStr] [bg, rect1, rect2]

view : Model -> Html Msg
view model =
    let
        fps = viewFPS model.fpsCounter
    in
    Html.body []
    [ Html.p [] [fps]
    , Html.hr [] []
    , viewSvgTest
    ] 
