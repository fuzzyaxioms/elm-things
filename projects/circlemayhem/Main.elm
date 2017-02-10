module Main exposing (main)

import Html exposing (Html)
import Svg exposing (Svg, Attribute)
import Svg.Attributes as SA
import Time exposing (Time)
import Color exposing (Color)
import String
import AnimationFrame
import Keyboard
import Char exposing (toCode)
import Platform.Sub as Sub


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

playerRadius : Float
playerRadius = 20.0

playerColor : Color
playerColor = Color.blue

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


type alias Player =
    { radius : Float
    , color : Color
    , x : Float
    , y : Float
    , speed : Float
    , tUp : ThrustState
    , tDown : ThrustState
    , tLeft : ThrustState
    , tRight : ThrustState
    }

newPlayer : Player 
newPlayer = Player playerRadius playerColor 0.0 0.0 300.0 ThrustOff ThrustOff ThrustOff ThrustOff

type alias Model = 
    { fpsCounter : FPSCounter
    , player : Player
    }

updatePlayer : Player -> Time -> Player
updatePlayer ({x, y, speed, tUp, tDown, tLeft, tRight} as player) diff = 
  let
    elapsed = Time.inMilliseconds diff
    dx = (elapsed * speed / 1000.0) * (thrustInd tRight - thrustInd tLeft)
    dy = (elapsed * speed / 1000.0) * (thrustInd tDown - thrustInd tUp)
    x1 = x + dx
    y1 = y + dy
  in
    {player | x = x1, y = y1}

newModel : Model
newModel =
    { fpsCounter = newFPSCounter
    , player = newPlayer
    }

initModel : Model
initModel = newModel

init : (Model, Cmd Msg)
init =
  ( initModel
  , Cmd.none
  )


-- UPDATE

type ThrustDirection
  = ThrustUp
  | ThrustDown
  | ThrustLeft
  | ThrustRight

type ThrustState
  = ThrustOn
  | ThrustOff

thrustInd : ThrustState -> Float
thrustInd t = case t of
  ThrustOn ->
    1.0
  ThrustOff ->
    0.0

type Msg
  = Noop -- sometimes need to react to an event but do nothing
  | Step Time
  | Thrust ThrustDirection ThrustState

updateThrust : Model -> ThrustDirection -> ThrustState -> Model
updateThrust ({player} as model) dir act =
  let
    player1 = case dir of
      ThrustUp ->
        {player | tUp = act}
      ThrustDown ->
        {player | tDown = act}
      ThrustLeft ->
        {player | tLeft = act}
      ThrustRight ->
        {player | tRight = act}
  in
    {model | player = player1}
    

update : Msg -> Model -> (Model, Cmd Msg)
update msg ({player,fpsCounter} as model) =
  case msg of
    Noop ->
      (model, Cmd.none)
    Thrust dir act ->
      (updateThrust model dir act, Cmd.none)
    Step diff ->
      ({model
        | fpsCounter = updateFPSCounter fpsCounter diff
        , player = updatePlayer player diff},
      Cmd.none)
    


-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
  let
    makeKeyHandler act keycode =
      if keycode == Char.toCode 'W' then
        Thrust ThrustUp act
      else if keycode == Char.toCode 'A' then
        Thrust ThrustLeft act
      else if keycode == Char.toCode 'S' then
        Thrust ThrustDown act
      else if keycode == Char.toCode 'D' then
        Thrust ThrustRight act
      else
        Noop
  in
    Sub.batch
    [ AnimationFrame.diffs <| Step << clamp minFrameDiff maxFrameDiff
    , Keyboard.downs <| makeKeyHandler ThrustOn
    , Keyboard.ups <| makeKeyHandler ThrustOff
    ]
  


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

drawPlayer : Player -> Svg Msg
drawPlayer {x,y,radius,color} = 
  let
    cx = SA.cx <| toString x
    cy = SA.cy <| toString y
    r = SA.r <| toString radius
    fillOpacity = SA.fillOpacity "0"
    stroke = SA.stroke "blue"
    strokeWidth = SA.strokeWidth "5px"
  in    
  Svg.circle [cx, cy, r, fillOpacity, stroke, strokeWidth] []

viewScene : Model -> Html Msg
viewScene model = 
    let
      width = 720
      height = 720
      wStr = toString width
      hStr = toString height
      viewStr = "0 0 " ++ wStr ++ " " ++ hStr
      bg = Svg.rect [SA.x "0", SA.y "0", SA.width wStr, SA.height hStr, SA.fill "black"]  []
      playerSprite = drawPlayer model.player
    in
      Svg.svg [SA.width wStr, SA.height hStr, SA.viewBox viewStr] [bg, playerSprite]

view : Model -> Html Msg
view model =
    let
        fps = viewFPS model.fpsCounter
    in
    Html.body []
    [ Html.p [] [fps]
    , Html.p [] [Html.text "WASD to move."]
    , Html.hr [] []
    , viewScene model
    ] 
