module LList exposing (LList)

{- List with length and other useful helper functions.
-}

import List

type LList a = LList (Int, List a)

length : LList a -> Int
length (LList (len, _)) = len

empty : LList a
empty = LList (0, [])

cons : a -> LList a -> LList a
cons x (LList (len, xs)) =
  LList (len+1, x :: xs)

singleton : a -> LList a
singleton x =
  LList (1, List.singleton x)

map : (a -> b) -> LList a -> LList b
map fn (LList (len, xs)) =
  LList (len, List.map fn xs)

filterMaybe : LList (Maybe a) -> LList a
filterMaybe (LList (_, xs)) =
  let
    maybecons : Maybe a -> LList a -> LList a
    maybecons m ll =
      case m of
        Nothing ->
          ll
        Just x  ->
          cons x ll   
  in
  List.foldr maybecons empty xs
