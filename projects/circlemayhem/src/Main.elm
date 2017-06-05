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
import Random
import List


main : Program Never Model Msg
main =
  Html.program
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }


-- Global parameters

type alias Model =
    { settings : Settings
    , fpsCounter : FPSCounter
    , player : Player
    , enemyManager : EnemyManager
    }

initModel : Model
initModel =
  let
    settings = initSettings
  in
  { settings = settings
  , fpsCounter = initFPSCounter
  , player = initPlayer settings
  , enemyManager = initEnemyManager
  }

type alias Settings =
  { maxFrameDiff : Float
  , minFrameDiff : Float
  , screenWidth : Float
  , screenHeight : Float
  , playerRadius : Float
  , playerColor : Color
  }

initSettings : Settings
initSettings =
  { maxFrameDiff = 100.0 -- milliseconds
  , minFrameDiff = 1e-4
  , screenWidth = 720.0
  , screenHeight = 720.0
  , playerRadius = 20.0
  , playerColor = Color.blue
  }

type alias FPSCounter = { frameTime : Float }

initFPSCounter : FPSCounter
initFPSCounter = {frameTime=1000.0/60.0}

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


clampCircleEntity : Float -> Float -> Float -> Float -> Float -> (Float, Float)
clampCircleEntity x y radius width height =
  let
    x1 = if x + radius > width then (width - radius) else if x - radius < 0 then radius else x
    y1 = if y + radius > width then (width - radius) else if y - radius < 0 then radius else y
  in
    (x1, y1)

type alias Player =
    { x : Float
    , y : Float
    , radius : Float
    , color : Color
    , speed : Float
    , tUp : ThrustState
    , tDown : ThrustState
    , tLeft : ThrustState
    , tRight : ThrustState
    }

type alias EnemyManager =
  { seed : Random.Seed
  , enemies : List Enemy
  , maxCount : Int -- maximum number of enemies
  , spawnLapse : Float -- seconds in-between enemy respanws
  , elapsed : Float -- keep track of elapsed time since last respawn
  }

initEnemyManager : EnemyManager
initEnemyManager = 
  { seed = Random.initialSeed 0x40e7b36c
  , enemies = []
  , maxCount = 20
  , spawnLapse = 5.0
  , elapsed = 5.0
  }

spawnEnemy : Settings -> EnemyAI -> Random.Seed -> (Enemy, Random.Seed)
spawnEnemy settings ai seed =
  Random.step (generateEnemy settings ai) seed

spawnEnemies : Settings -> EnemyAI -> Int -> Random.Seed -> (List Enemy, Random.Seed)
spawnEnemies settings ai num seed =
  let
    acc n s out =
      if n > 0 then
        let
          (newE, newS) = spawnEnemy settings ai s
        in
          acc (n-1) newS (newE::out)
      else
        (out, s)
  in
    acc num seed []

spawnNewEnemies : Settings -> Random.Seed -> (Random.Seed, List Enemy)
spawnNewEnemies settings seed =
  let
    seed1 = seed
    (newNormals, seed2) = spawnEnemies settings Normal 4 seed1
    (newHomings, seed3) = spawnEnemies settings Homing 4 seed2
    (newRandoms, seed4) = spawnEnemies settings Random 4 seed3
  in
    (seed4, List.concat [newNormals, newHomings, newRandoms])

updateEnemyManager : EnemyManager -> Settings -> Time -> EnemyManager
updateEnemyManager eman settings diff =
  let
    diffSeconds = Time.inSeconds diff
    elapsed1 = eman.elapsed + diffSeconds
    numEnemies = List.length eman.enemies
    (enemies1, seed1, elapsed2) =
      if elapsed1 >= eman.spawnLapse then
        if numEnemies < eman.maxCount then
          let
            (seed1, enemies1) = spawnNewEnemies settings eman.seed
          in
            (eman.enemies ++ enemies1, seed1, elapsed1 - eman.spawnLapse)
        else
          (eman.enemies, eman.seed, elapsed1 - eman.spawnLapse)
      else
        (eman.enemies, eman.seed, elapsed1)
    enemies2 = List.map (updateEnemy settings diff) enemies1
  in
    {eman
      | elapsed = elapsed2
      , enemies = enemies2
      , seed = seed1
    }

type EnemyAI
  = Normal -- starts with random velocity and just bounces around
  | Homing -- aims towards the player
  | Random -- same as normal but has some probability to randomly change direction

type alias Enemy =
  { x : Float
  , y : Float
  , radius : Float
  , dx : Float
  , dy : Float
  , ai : EnemyAI
  }

newEnemy : EnemyAI -> Float -> Float -> Float -> Float -> Enemy
newEnemy ai x y speed angle =
  { x = x
  , y = y
  , radius = 30.0
  , dx = speed * cos angle
  , dy = speed * sin angle
  , ai = ai
  }

generateEnemy : Settings -> EnemyAI -> Random.Generator Enemy
generateEnemy settings ai = 
  let
    genX = Random.float 0.0 settings.screenWidth
    genY = Random.float 0.0 settings.screenHeight
    genSpeed = Random.float 100.0 300.0
    genAngle = Random.float 0.0 (2.0 * pi)
  in
    Random.map4 (newEnemy ai) genX genY genSpeed genAngle

updateEnemy : Settings -> Time -> Enemy -> Enemy
updateEnemy {screenWidth, screenHeight} diff ({x, y, radius, dx, dy, ai} as enemy) = 
  let
    elapsed = Time.inMilliseconds diff
    (x1, y1) = case ai of -- TODO implement the other AI behaviors
      _ ->
        (x + (elapsed * dx / 1000.0), y + (elapsed * dy / 1000.0))
    (x2, dx2) = if x1 + radius > screenWidth then (screenWidth-radius, -dx) else if (x1-radius < 0) then (radius,-dx) else (x1,dx)
    (y2, dy2) = if y1 + radius > screenHeight then (screenHeight-radius, -dy) else if (y1-radius < 0) then (radius,-dy) else (y1,dy)
    enemy1 = {enemy | x = x2, y = y2, dx=dx2, dy=dy2}
  in
    enemy1

initPlayer : Settings -> Player 
initPlayer {playerRadius, playerColor} =
  { x=0.0
  , y=0.0
  , radius=playerRadius
  , color=playerColor
  , speed=300.0
  , tUp=ThrustOff
  , tDown=ThrustOff
  , tLeft=ThrustOff
  , tRight=ThrustOff
  }

updatePlayer : Player -> Settings -> Time -> Player
updatePlayer ({x, y, radius, speed, tUp, tDown, tLeft, tRight} as player) {screenWidth, screenHeight} diff = 
  let
    elapsed = Time.inMilliseconds diff
    dx = (elapsed * speed / 1000.0) * (thrustInd tRight - thrustInd tLeft)
    dy = (elapsed * speed / 1000.0) * (thrustInd tDown - thrustInd tUp)
    x1 = x + dx
    y1 = y + dy
    (x2, y2) = clampCircleEntity x1 y1 radius screenWidth screenHeight
    player1 = {player | x = x2, y = y2}
  in
    player1

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
update msg ({settings,player,fpsCounter,enemyManager} as model) =
  case msg of
    Noop ->
      (model, Cmd.none)
    Thrust dir act ->
      (updateThrust model dir act, Cmd.none)
    Step diff ->
      ({model
        | fpsCounter = updateFPSCounter fpsCounter diff
        , player = updatePlayer player settings diff
        , enemyManager = updateEnemyManager enemyManager settings diff
        },
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
    [ AnimationFrame.diffs <| Step << clamp model.settings.minFrameDiff model.settings.maxFrameDiff
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

drawEnemy : Enemy -> Svg Msg
drawEnemy {x,y,ai,radius} = 
  let
    cx = SA.cx <| toString x
    cy = SA.cy <| toString y
    r = SA.r <| toString radius
    fillOpacity = SA.fillOpacity "0"
    stroke = 
      case ai of
        Normal ->
          SA.stroke "red"
        Homing ->
          SA.stroke "magenta"
        Random ->
          SA.stroke "orange"
    strokeWidth = SA.strokeWidth "5px"
  in    
  Svg.circle [cx, cy, r, fillOpacity, stroke, strokeWidth] []

drawEnemies : EnemyManager -> Svg Msg
drawEnemies {enemies} =
  Svg.g [] (List.concat [List.map drawEnemy enemies])

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
      enemies = drawEnemies model.enemyManager
    in
      Svg.svg [SA.width wStr, SA.height hStr, SA.viewBox viewStr] [bg, playerSprite, enemies]

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
