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
    , balls : List Ball
    , numFrames : Int
    , numMS : Time
    , fps : Float
    }

randBall w h =
    let 
        randR = Random.map toFloat <| Random.int 4 50
        randX = Random.map toFloat <| Random.int 0 w 
        randY = Random.map toFloat <| Random.int 0 h 
        randDx = Random.map toFloat <| Random.int 4 20
        randDy = Random.map toFloat <| Random.int 4 20
    in
    Random.map5 (\r x y dx dy -> {x=x, y=y, dx=dx, dy=dy, r=r}) randR randX randY randDx randDy

init : (Model, Cmd Msg)
init =
  ( { canvasHeight=600
     , canvasWidth=600
     , balls = []
     , numFrames = 0
     , numMS = 0
     , fps = 0
     }
  , (Random.list 20 (randBall 600 600) ) |> Random.generate Construct
  )


-- UPDATE

type Msg
  = Construct (List Ball)
  | Step Time


updateBall : Int -> Int -> Ball -> Ball
updateBall w_ h_ b =
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

teleportBall : Ball -> Float -> Float -> Ball
teleportBall b x y =
    {b | x = x, y = y}

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Construct balls ->
        ({model | balls = balls}, Cmd.none)

    Step diff ->
        let
            newNumFrames = model.numFrames + 1
            newNumMS = model.numMS + diff
            (newNumFrames2, newNumMS2, newFPS) = if Time.inSeconds newNumMS >= 1.0 then (0,0, newNumFrames / Time.inSeconds newNumMS) else (newNumFrames, newNumMS, model.fps)
        in
      ({model | balls = List.map (updateBall model.canvasWidth model.canvasHeight) model.balls, numFrames=newNumFrames2, numMS=newNumMS2, fps=newFPS},
      Cmd.none)


-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
  AnimationFrame.diffs Step


-- VIEW

viewBall : Ball -> Svg Msg
viewBall b =
    circle [ cx (toString b.x), cy (toString b.y), r (toString b.r), fill "#0B79CE" ] []

view : Model -> Html Msg
view model =
    Html.div [] [
        svg [ width (toString model.canvasWidth), height (toString model.canvasHeight) ]
          ([
              rect [ x "0", y "0", width (toString model.canvasWidth), height (toString model.canvasHeight)] []
          ] ++ List.map viewBall model.balls)
      , Html.hr [] []
      , Html.text  (toString model.fps)
      ]
