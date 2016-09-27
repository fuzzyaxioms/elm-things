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

type alias Ball =
    { x : Float
    , y : Float
    , dx : Float
    , dy : Float
    , r : Float  
    }

type alias Model =
    { canvasHeight : Int
    , canvasWidth : Int
    , ball : Ball
    , numFrames : Int
    , numMS : Time
    , fps : Float
    }


init : (Model, Cmd Msg)
init =
  ( { canvasHeight=600
     , canvasWidth=600
     , ball = { r=10
              , x=200
              , y=100
              , dx=10
              , dy=10
     }
     , numFrames = 0
     , numMS = 0
     , fps = 0
     }
  , (Random.list 2 (Random.int 0 600) ) |> Random.generate (\rs ->
      case rs of
          [x,y] -> Teleport (toFloat x)  (toFloat y)
          _ -> Debug.crash "impossible"
      )
  )


-- UPDATE

type Msg
  = Step Time
  | Teleport Float Float


updateBall : Ball -> Int -> Int -> Ball
updateBall b w_ h_ =
    let
        w = toFloat w_
        h = toFloat h_
        x2 = b.x + b.dx
        y2 = b.y + b.dy
        dx2 = if x2 <= b.r || x2 + b.r >= w then -b.dx else b.dx
        dy2 = if y2 <= b.r || y2 + b.r >= h then -b.dy else b.dy
        x3 = clamp b.r (w - b.r) x2
        y3 = clamp b.r (h - b.r) y2
    in
    {b | x = x3, y = y3, dx = dx2, dy = dy2}

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Step diff ->
        let
            
            newNumFrames = model.numFrames + 1
            newNumMS = model.numMS + diff
            (newNumFrames2, newNumMS2, newFPS) = if Time.inSeconds newNumMS >= 1.0 then (0,0, newNumFrames / Time.inSeconds newNumMS) else (newNumFrames, newNumMS, model.fps)
        in
      ({model | ball = updateBall model.ball model.canvasWidth model.canvasHeight, numFrames=newNumFrames2, numMS=newNumMS2, fps=newFPS},
      Cmd.none)

    Teleport x y ->
        let
            b = model.ball
            newBall = {b | x = x, y = y}
        in
        ({model | ball = newBall}, Cmd.none)


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
              , circle [ cx (toString model.ball.x), cy (toString model.ball.y), r (toString model.ball.r), fill "#0B79CE" ] []
          ]
      , Html.hr [] []
      , Html.text  (toString model.fps)
      ]
