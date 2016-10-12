module Main exposing (main)

import Html exposing (Html)
import Html.App as App
import Html.Events exposing (onSubmit,onInput,onClick)
import Html.Attributes as Attr
import Time exposing (Time)
import Task
import Color exposing (Color)
import Math.Vector2 exposing (..)
import String
import AnimationFrame
import Random.Pcg as Random exposing (Generator, Seed)

import Collage exposing (Form)
import Element exposing (Element)
import Transform


main : Program Never
main =
  App.program
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }


-- MODEL

-- Ball stuff
type alias Ball =
    { pos : Vec2
    , vel : Vec2
    , r : Float
    , clr : Color
    }

newBall : Float -> Float -> Float -> Float -> Float -> Color -> Ball
newBall r x y dx dy clr = {pos=vec2 x y, vel=vec2 dx dy, r=r, clr=clr}

randColor : Generator Color
randColor =
    let
        r1 = Random.int 0 255
        r2 = Random.int 0 255
        r3 = Random.int 0 255
    in
    Random.map3 Color.rgb r1 r2 r3

randBall : Int -> Int -> Generator Ball
randBall w h =
    let
        randR = Random.map toFloat <| Random.int 5 50
        randX = Random.map toFloat <| Random.int 0 w
        randY = Random.map toFloat <| Random.int 0 h
        randDx = Random.map toFloat <| Random.int 5 200
        randDy = Random.map toFloat <| Random.int 5 200
    in
    newBall `Random.map` randR `Random.andMap` randX `Random.andMap` randY `Random.andMap` randDx `Random.andMap` randDy `Random.andMap` randColor


updateBall : Int -> Int -> Time -> Ball -> Ball
updateBall w_ h_ dt b =
    let
        w = toFloat w_
        h = toFloat h_
        (x,y) = toTuple <| add b.pos <| scale (Time.inSeconds dt) b.vel
        (dx,dy) = toTuple b.vel
        (x2,dx2) = if x <= b.r then (b.r, -dx) else if x + b.r >= w then (w - b.r, -dx) else (x, dx)
        (y2,dy2) = if y <= b.r then (b.r, -dy) else if y + b.r >= h then (h - b.r, -dy) else (y, dy)
    in
    {b | pos = vec2 x2 y2, vel = vec2 dx2 dy2}

viewBall : Ball -> Form
viewBall b =
    Collage.move (toTuple b.pos) <| Collage.filled b.clr <| Collage.circle b.r

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
        w = 1.0 / (1.0 + e^(-s))
    in
    {fps = (1.0-w)*fps + w * (1.0 / s)}

type alias Model =
    { canvasHeight : Int
    , canvasWidth : Int
    , balls : List Ball
    , fpsCounter : FPSCounter
    , inputNum : Int
    }


init : (Model, Cmd Msg)
init =
  ( { canvasHeight=600
     , canvasWidth=600
     , balls = []
     , fpsCounter = newFPSCounter
     , inputNum = 20
     }
  , (Random.list 20 (randBall 600 600) ) |> Random.generate Construct
  )


-- UPDATE

type Msg
  = Generate
  | UpdateNum Int
  | Construct (List Ball)
  | Step Time

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Generate ->
        (model, (Random.list model.inputNum (randBall 600 600) ) |> Random.generate Construct)
    
    UpdateNum n ->
        ({model | inputNum = n}, Cmd.none)

    Construct balls ->
        ({model | balls = balls}, Cmd.none)

    Step diff ->
      ({model | balls =
        List.map (updateBall model.canvasWidth model.canvasHeight diff) model.balls,
        fpsCounter = updateFPSCounter model.fpsCounter diff},
      Cmd.none)


-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
  AnimationFrame.diffs (Step << clamp 0.0 100.0)


-- VIEW

viewFPS : Float -> Html Msg
viewFPS fps =
    let 
        n = round (fps * 100)
        a = toString (n // 100)
        b = String.padLeft 2 '0' <| toString (n % 100) 
    in
    Html.text <| "FPS: " ++ a ++ "." ++ b 

viewCanvas : Model -> Html Msg
viewCanvas model =
    let
        repos = Transform.translation (-(toFloat model.canvasWidth)/2) (-(toFloat model.canvasHeight)/2)
        background = Collage.filled Color.black <| Collage.rect (toFloat model.canvasWidth) (toFloat model.canvasHeight)
        forms = Collage.groupTransform repos <| List.map viewBall model.balls
    in
    Element.toHtml <| Collage.collage model.canvasWidth model.canvasHeight [background,forms]

view : Model -> Html Msg
view model =
    let
        canvas = viewCanvas model
        fps = viewFPS <| getFPS model.fpsCounter
        inputNum = Html.input [onInput (UpdateNum << Result.withDefault 20 << String.toInt)] []
        resetButton = Html.button [onClick Generate] [Html.text "Generate"]
    in
    Html.body [] [
        canvas
        , Html.hr [] []
        , Html.p [] [fps]
        , Html.p [] [Html.text "Enter number of balls: ", inputNum,resetButton]
    ] 
