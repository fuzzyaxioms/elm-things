module FPSCounter exposing (FPSCounter,init,fps,update)

import Time exposing (Time)

type alias FPSCounter =
    { frameTime : Float
    , timeWindow : Float -- approximate window over which we average
    }

init : FPSCounter
init = {frameTime=1.0, timeWindow=1.0}

fps : FPSCounter -> Float
fps {frameTime} = 1.0 / frameTime

-- use simple exponential decay, but weighted accordingly to duration
update : FPSCounter -> Time -> FPSCounter
update ({frameTime,timeWindow} as counter) diff =
    let
        diffSeconds = Time.inSeconds diff
        ratio = (min diffSeconds timeWindow) / timeWindow
        weight = 2 ^ (logBase 2 0.01 * ratio)
        frameTime2 = weight * frameTime + (1.0 - weight) * diffSeconds
    in
    {counter | frameTime = frameTime2}


