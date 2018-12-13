module UsingCollage exposing (..)

import ListExtra exposing (..)
import Element exposing (Element, toHtml)
import Collage exposing (..)
import Color exposing (..)
import Fibonacci exposing (..)
import Mouse exposing (Position, moves)
import Window exposing (Size, resizes, size)
import Task exposing (..)
import Array exposing (..)
import Html
import Circle exposing (..)
import Process exposing (..)
import Time exposing (..)


main : Program Never Model Msg
main =
    Html.program
        { init = init (fibonacci 10 1 1)
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type alias Model =
    { circles : Array Circle
    , windowSize : Size
    , delays : List Int
    }


init : List Int -> ( Model, Cmd Msg )
init radiis =
    let
        windowSize =
            Size 10 10

        drawPosition =
            createDrawPosition windowSize (Position 0 0)

        delays =
            List.map ((*) 100) radiis

        circles =
            List.map (Circle drawPosition) radiis
                |> fromList
    in
        ( Model circles windowSize delays, perform Resize Window.size )


type Msg
    = MouseMoved Position
    | Resize Size
    | Draw ( Int, Point2 )


subscriptions model =
    Sub.batch
        [ Mouse.moves MouseMoved
        , Window.resizes Resize
        ]


createDrawPosition : Size -> Position -> Point2
createDrawPosition windowSize position =
    let
        xPos =
            (position.x - round (toFloat windowSize.width / 2))

        yPos =
            (round (toFloat windowSize.height / 2)) - position.y
    in
        Point2 xPos yPos

delay : Time -> Task error value -> Task error value
delay time task =
  sleep time |> andThen (\_ -> task)
    

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        MouseMoved position ->
            let
                draw =
                    createDrawPosition model.windowSize position
                        |> List.repeat (length model.circles)
                        |> List.map2 (,) (List.range 0 (length model.circles))
                        |> List.map Task.succeed
                        |> List.map2 delay (List.map toFloat model.delays)
                        |> List.map (perform Draw)
            in
                ( model, Cmd.batch draw )

        Resize windowSize ->
            ( { model | windowSize = windowSize }, Cmd.none )

        Draw ( index, position ) ->
            let
                updateCirclePosition center circle =
                    { circle | center = center }

                circle =
                    get index model.circles

                updatedCircle =
                    Maybe.map (updateCirclePosition position) circle
            in
                case updatedCircle of
                    Nothing ->
                        Debug.crash "Unexpected circle index"

                    Just circle ->
                        let
                            circles =
                                set index circle model.circles
                        in
                            ( { model | circles = circles }, Cmd.none )


view : Model -> Html.Html Msg
view model =
    toList model.circles
        |> List.reverse
        |> drawCircles model.windowSize
        |> toHtml


drawCircle : Int -> Circle -> Form
drawCircle colourIndex circleData =
    circle (toFloat circleData.radius)
        |> filled (getColour colourIndex)
        |> move ( toFloat circleData.center.x, toFloat circleData.center.y )


drawCircles : Window.Size -> List Circle -> Element
drawCircles windowSize circlesData =
    let
        colorIndices =
            List.range 1 (List.length circlesData)

        circles =
            List.map2 drawCircle colorIndices circlesData
    in
        collage windowSize.width windowSize.height circles


colours : List Color
colours =
    [ red, green, orange, purple, yellow, blue, brown ]


getColour : Int -> Color
getColour index =
    Maybe.withDefault Color.green (colours !! (index % (List.length colours)))
