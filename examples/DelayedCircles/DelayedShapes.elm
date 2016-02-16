import Graphics.Element exposing(..)
import Graphics.Collage exposing(..)
import Color exposing (..)
import Mouse
import Window
import Time
import Signal.Extra exposing (..)
import Fibonacci exposing (..)
import ListExtra exposing (..)

main : Signal Element
main =
  let
    radiis = fibonacci 10 1 1
    delays = radiis
    drawingPositions = createDrawPositions <~ Window.dimensions ~ (delayedMousePositions delays)
    drawingPositionsWithRadius = attachRadius <~ (Signal.constant radiis) ~ drawingPositions
  in
    displayCircles <~ Window.dimensions ~ drawingPositionsWithRadius

{--
combine : List (Signal a) -> Signal (List a)
combine =
  List.foldr (Signal.map2 (::)) (Signal.constant [])
--}

createDrawPositions : (Int, Int) -> List(Int, Int) -> List(Float, Float)
createDrawPositions (w, h) positions =
  List.map (\(x, y) -> (toFloat x - toFloat w / 2, toFloat h / 2 - toFloat y)) positions

attachRadius : List Int -> List (Float, Float) -> List (Int, (Float, Float))
attachRadius radiis positions =
  List.map2 (,) radiis positions

{--}
delayedMousePositions : List Int -> Signal (List (Int, Int))
delayedMousePositions delays =
  let
    delayedPosition delay =
      Time.delay (Time.millisecond * toFloat(delay) * 50) Mouse.position
  in
    List.map delayedPosition delays |> Signal.Extra.combine
--}

colours = [ red, green, orange, purple, yellow, blue, brown ]

getColour : Int -> Color
getColour index =
  Maybe.withDefault Color.green <| (colours !! (index % (List.length colours)))

drawCircle : Int -> (Int, (Float, Float)) -> Form
drawCircle colourIndex (radius, (x, y)) =
  circle (toFloat radius)
  |> filled (getColour colourIndex)
  |> move (x, y)

displayCircles : (Int, Int) -> List (Int, (Float, Float)) -> Element
displayCircles (w, h) circlesData =
  let
    data = List.map2 (,) [1..(List.length circlesData)] (List.reverse circlesData)
  in
    collage w h (List.map (\x -> uncurry drawCircle x) data)

{--
scene : (Int, Int) -> (Int, Int) -> (Int, Int) -> Element
scene (w, h) position1 position2 =
  let
    drawCircle (x, y) (radius, colour) =
      circle radius
      |> filled colour
      |> move (toFloat x - toFloat w / 2, -(toFloat y - toFloat h / 2))
    differentRadius = [100, 70]--, 40, 25, 15, 5]
    differentColour = [ red, green]--, yellow, purple, blue, darkOrange ]
    radiusAndColour = List.map2 (,) differentRadius differentColour
  in
    collage w h [(drawCircle position1 (100, red)),  (drawCircle position2 (70, green))]


clickPositions = Signal.foldp (::) [] (Signal.sampleOn Mouse.clicks Mouse.position)

scene : (Int, Int) -> List (Int, Int) -> Element
scene (w, h) positions =
  let drawCircle (x, y) =
    circle 50
    |> filled green
    |> move (toFloat x - toFloat w / 2, -(toFloat y - toFloat h / 2))
  in
    collage w h (List.map drawCircle positions)
--}
