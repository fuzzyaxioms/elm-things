module Main exposing (main)

import Html exposing (Html)
import Html.Events exposing (onSubmit,onInput,onClick)
import Html.Attributes as Attr
import Svg exposing (Svg, Attribute)
import Svg.Attributes as SAttr
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


main : Program Never Model Msg
main =
  Html.program
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }


colorToString : Color -> String
colorToString clr = 
    let 
        {red,green,blue} = Color.toRgb clr
    in
    "rgb(" ++ toString red ++ "," ++ toString green ++ "," ++ toString blue ++ ")" 

-- MODEL

-- Ball stuff
type alias Ball =
    { pos : Vec2
    , vel : Vec2
    , r : Float
    , clr : Color
    , form : Form -- see if we can speed things up by partially generating the filled circle form
    , svg : Svg Msg -- see if we can speed things up
    }

newBall : Float -> Float -> Float -> Float -> Float -> Color -> Ball
newBall r x y dx dy clr = {pos=vec2 x y, vel=vec2 dx dy, r=r, clr=clr, form=Collage.filled clr <| Collage.circle r, svg=Svg.circle [SAttr.r <| toString r, SAttr.color <| colorToString clr] []}

randColor : Generator Color
randColor =
    let
        r1 = Random.int 0 255
        r2 = Random.int 0 255
        r3 = Random.int 0 255
    in
    Random.map3 Color.rgb r1 r2 r3

randBall : Model -> Generator Ball
randBall model =
    let
        randR = Random.map toFloat <| Random.int 5 50
        randX = Random.map toFloat <| Random.int (round model.leftBound) (round model.rightBound)
        randY = Random.map toFloat <| Random.int (round model.downBound) (round model.upBound)
        randDx = Random.map toFloat <| Random.int 5 200
        randDy = Random.map toFloat <| Random.int 5 200
        andM = flip Random.andMap
    in
    Random.constant newBall |> andM randR |> andM randX |> andM randY |> andM randDx |> andM randDy |> andM randColor


updateBall : Model -> Time -> Ball -> Ball
updateBall m dt b =
    let
        (x,y) = toTuple <| add b.pos <| scale (Time.inSeconds dt) b.vel
        (dx,dy) = toTuple b.vel
        (x2,dx2) = if x - b.r <= m.leftBound then (m.leftBound + b.r, -dx) else if x + b.r >= m.rightBound then (m.rightBound - b.r, -dx) else (x, dx)
        (y2,dy2) = if y - b.r <= m.downBound then (m.downBound + b.r, -dy) else if y + b.r >= m.upBound then (m.upBound - b.r, -dy) else (y, dy)
    in
    {b | pos = vec2 x2 y2, vel = vec2 dx2 dy2}

viewBall : Ball -> Form
viewBall b =
    Collage.move (toTuple b.pos) <| b.form

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
    { canvasHeight : Int
    , canvasWidth : Int
    , leftBound : Float
    , rightBound : Float
    , upBound : Float
    , downBound : Float
    , balls : List Ball
    , fpsCounter : FPSCounter
    , inputNum : Int
    }

newModel : Int -> Int -> Model
newModel w h =
    { canvasHeight=h
     , canvasWidth=w
     , leftBound = 0
     , rightBound = toFloat w
     , upBound = toFloat h
     , downBound = 0
     , balls = []
     , fpsCounter = newFPSCounter
     , inputNum = 20
     }

initModel : Model
initModel = newModel 600 600

init : (Model, Cmd Msg)
init =
  ( initModel
  , (Random.list 20 (randBall initModel) ) |> Random.generate Construct
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
        (model, (Random.list model.inputNum (randBall model) ) |> Random.generate Construct)
    
    UpdateNum n ->
        ({model | inputNum = n}, Cmd.none)

    Construct balls ->
        ({model | balls = balls}, Cmd.none)

    Step diff ->
        let 
            newBalls = List.map (updateBall model diff) model.balls
        in
      ({model | balls = newBalls,
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
        background = Collage.filled Color.black <| Collage.rect (toFloat model.canvasWidth) (toFloat model.canvasHeight)
        forms = List.map viewBall model.balls
    in
    Element.toHtml <| Collage.collage model.canvasWidth model.canvasHeight (background :: forms)

viewCanvasSvg : Model -> Html Msg
viewCanvasSvg model =
    let 
        viewBall b = Svg.circle [SAttr.r <| toString b.r, SAttr.style <| ("fill:" ++ colorToString b.clr), SAttr.cx <| toString <| getX b.pos, SAttr.cy <| toString <| getY b.pos] []
        balls = List.map viewBall model.balls
    in
    Svg.svg [SAttr.width <| toString model.canvasWidth, SAttr.height <| toString model.canvasHeight, SAttr.viewBox <| "0 0 600 600"] balls

view : Model -> Html Msg
view model =
    let
        canvas = viewCanvasSvg model
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
