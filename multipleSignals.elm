import Html exposing(..)
import Graphics.Element exposing(..)
import Graphics.Collage exposing(..)
import Color exposing (..)
import Mouse
import Window
import Time
import Signal.Extra exposing (..)
import core-extra

main =
  let
    signal1 = (Time.delay Time.second Mouse.position)
    signal2 = (Time.delay (Time.second / 2) Mouse.position)
  in
    scene <~ signal1 ~ signal2

scene (x1, y1) (x2, y2) =
  flow Graphics.Element.right [show (x1, y1), show (x2, y2)]
