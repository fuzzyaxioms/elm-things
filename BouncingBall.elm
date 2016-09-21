module BouncingBall exposing (main)

import Html exposing (Html)
import Html.App as App
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Time exposing (Time)
import Date exposing (Date)
import Task
import String
import AnimationFrame


main =
  App.program
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }


-- MODEL

type alias Model =
    { canvasHeight : Int
    , canvasWidth : Int
    , ballRadius : Int
    , ballX : Int
    , ballY : Int
    , ballVelocityX : Int
    , ballVelocityY : Int
    , numFrames : Int
    , numMS : Time
    , fps : Float
    }


init : (Model, Cmd Msg)
init =
  ( { canvasHeight=400
     , canvasWidth=400
     , ballRadius=10
     , ballX=200
     , ballY=100
     , ballVelocityX=10
     , ballVelocityY=10
     , numFrames = 0
     , numMS = 0
     , fps = 0
     }
  , Cmd.none)


-- UPDATE

type Msg
  = Step Time


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Step diff ->
        let
            newX = model.ballX + model.ballVelocityX
            newY = model.ballY + model.ballVelocityY
            newVelX = if newX <= model.ballRadius || newX + model.ballRadius >= model.canvasWidth then -model.ballVelocityX else model.ballVelocityX
            newVelY = if newY <= model.ballRadius || newY + model.ballRadius >= model.canvasHeight then -model.ballVelocityY else model.ballVelocityY
            newX2 = clamp model.ballRadius (model.canvasWidth - model.ballRadius) newX
            newY2 = clamp model.ballRadius (model.canvasHeight - model.ballRadius) newY
            newNumFrames = model.numFrames + 1
            newNumMS = model.numMS + diff
            (newNumFrames2, newNumMS2, newFPS) = if Time.inSeconds newNumMS >= 1.0 then (0,0, newNumFrames / Time.inSeconds newNumMS) else (newNumFrames, newNumMS, model.fps)
        in
      ({model | ballX = newX2, ballY = newY2, ballVelocityX = newVelX, ballVelocityY = newVelY, numFrames=newNumFrames2, numMS=newNumMS2, fps=newFPS},
      Cmd.none)


-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
  AnimationFrame.diffs Step


-- VIEW

view : Model -> Html Msg
view model =
    Html.div [] [
        svg [ viewBox "0 0 400 400", width "400px" ]
          [ circle [ cx (toString model.ballX), cy (toString model.ballY), r (toString model.ballRadius), fill "#0B79CE" ] []
          ]
      , Html.hr [] []
      , Html.text  (toString model.fps)
      ]
