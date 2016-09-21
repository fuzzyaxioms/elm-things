module BouncingBall exposing (main)

import Html exposing (Html)
import Html.App as App
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Time exposing (Time, second)
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
    }


init : (Model, Cmd Msg)
init =
  ( { canvasHeight=400
     , canvasWidth=400
     , ballRadius=10
     , ballX=200
     , ballY=100
     , ballVelocityX=10
     , ballVelocityY=10 }
  , Cmd.none)


-- UPDATE

type Msg
  = Step


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Step ->
        let
            newX = model.ballX + model.ballVelocityX
            newY = model.ballY + model.ballVelocityY
            newVelX = if newX <= model.ballRadius || newX + model.ballRadius >= model.canvasWidth then -model.ballVelocityX else model.ballVelocityX
            newVelY = if newY <= model.ballRadius || newY + model.ballRadius >= model.canvasHeight then -model.ballVelocityY else model.ballVelocityY
            newX2 = clamp model.ballRadius (model.canvasWidth - model.ballRadius) newX
            newY2 = clamp model.ballRadius (model.canvasHeight - model.ballRadius) newY
        in
      ({model | ballX = newX2, ballY = newY2, ballVelocityX = newVelX, ballVelocityY = newVelY},
      Cmd.none)


-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
  AnimationFrame.diffs (always Step)


-- VIEW

view : Model -> Html Msg
view model =
    svg [ viewBox "0 0 400 400", width "400px" ]
      [ circle [ cx (toString model.ballX), cy (toString model.ballY), r (toString model.ballRadius), fill "#0B79CE" ] []
      ]
