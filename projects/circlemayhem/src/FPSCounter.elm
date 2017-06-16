module FPSCounter exposing (FPSCounter,init,fps,update)

{- How do we maintain an FPS counter.

Simple Approach: Exponential Decay/Weighted Update
We maintain an 'average' frame time, i.e. the time elapsed between frames.
Then FPS is simply the reciprocal of the frame time.
Let t be the average frame time. Then we update the frame time as follows:
t <- (1-w)*t + w*t'
for some weight w and the next frame time t'.
Now the question is what should w be? Should it be fixed or adaptive?
w controls how fast we adapt to the new data.
A large w means we want to update faster.
We want to simulate a running average over a fixed period of time, like 2s.
This means that if t' is large, then the FPS has slowed and we should make
a large update by using a large w. So by this logic, it seems like w should
be adaptive and should be related with t'.
Suppose t'=2s, then we want w = 1, to immediately update the FPS completely.
If t'=1s, then we would want w to be such that two updates change the FPS completely.
In general, we want w to be such that (2s/t') many updates are needed to change the FPS.
So now we need to figure out how to set w such that approximately that many updates can result in the correct change.

Suppose our current frame time is t, and we will keep getting the same t' in the future. The question is with a fixed w, how many updates do we need before we get close enough to t'? Let's figure this out.

t1 = (1-w)t + wt'
t2 = (1-w)t1 + wt'
   = (1-w)((1-w)t + wt') + wt'
   = t(1-w)^2 + (1-w)wt' + wt'
t3 = (1-w)t2 + wt'
   = (1-w)(t(1-w)^2 + (1-w)wt' + wt') + wt'
   = t(1-w)^3 + t'w(1-w)^2 + t'w(1-w) + t'w
   = t(1-w)^3 + t'[w(1-w)^2 + w(1-w) + w]
tn = t(1-w)^n + t'w[sum (1-w)^i for i=0 to n-1]

So (1-w)^n can be considered how much we decay the old value. So if we want relative error, we can set a bound on (1-w)^n. If we want absolute error, we can set a bound on t(1-w)^n.

Also, let's determine what n should be. Since we want to emulate a running average, we will have a window parameter d. The number of times we want to update with t' should be (d/t'). So
n =  so we don't update fewer times

So let's go with the absolute error bound.
t(1-w)^n = B
=> log t + n log (1-w) = log B
=> log (1 - w) = (log B - log t) / n
=> w = 1 - 2^[(log2 B - log2 t) / n]
=> w = 1 - 2^[(log2 B - log2 t) / max(d/t', 1)]
=> w = 1 - 2^[t' (log2 B - log2 t) / min(d, t')]
=> w = 1 - 2^[t' (log2 (B / t)) / min(d, t')]
-}

import Time exposing (Time)

type alias FPSCounter =
  { frameTime : Float -- in milliseconds
  , timeWindow : Float -- approximate window over which we average
  , absBound : Float -- absolute bound B in milliseconds 
  }

init : FPSCounter
init = {frameTime=10.0, timeWindow=500.0, absBound=0.1}

fps : FPSCounter -> Float
fps {frameTime} = 1000.0 / frameTime

update : FPSCounter -> Time -> FPSCounter
update ({frameTime,timeWindow,absBound} as counter) diff =
  let
    diffMilliseconds = Time.inMilliseconds diff
    ratio = (min diffMilliseconds timeWindow) / timeWindow
    weight1m = 2 ^ (ratio * (logBase 2 (absBound / diffMilliseconds)))
    frameTime2 = weight1m * frameTime + (1.0 - weight1m) * diffMilliseconds
  in
  {counter | frameTime = frameTime2}


