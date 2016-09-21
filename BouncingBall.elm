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
import Random.Pcg as Random


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
  ( { canvasHeight=600
     , canvasWidth=600
     , ballRadius=10
     , ballX=200
     , ballY=100
     , ballVelocityX=10
     , ballVelocityY=10
     , numFrames = 0
     , numMS = 0
     , fps = 0
     }
  , (Random.list 2 (Random.int 0 600) ) |> Random.generate (\rs ->
      case rs of
          [x,y] -> Teleport x  y
          _ -> Debug.crash "impossible"
      )
  )


-- UPDATE

type Msg
  = Step Time
  | Teleport Int Int


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

    Teleport x y ->
        ({model | ballX = x, ballY = y}, Cmd.none)


-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
  AnimationFrame.diffs Step


-- VIEW

view : Model -> Html Msg
view model =
    Html.div [] [
        svg [ width (toString model.canvasWidth), height (toString model.canvasHeight) ]
          [
              rect [ x "0", y "0", width (toString model.canvasWidth), height (toString model.canvasHeight)] []
              , circle [ cx (toString model.ballX), cy (toString model.ballY), r (toString model.ballRadius), fill "#0B79CE" ] []
          ]
      , Html.hr [] []
      , Html.text  (toString model.fps)
      ]
