module Main exposing (main)

import Browser
import Browser.Events
import Html exposing (Html)
import Html.Attributes
import Json.Decode as Decode exposing (Decoder)
import LineSegment2d exposing (LineSegment2d)
import Point2d exposing (Point2d)
import Svg
import Svg.Attributes
import Vector2d exposing (Vector2d)


marbleRadius =
    80


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }


type alias Model =
    { lines : List LineSegment2d
    , marble : Marble
    }


type alias Marble =
    { centerPoint : Point2d
    , velocity : Vector2d
    , radius : Float
    }


init : flags -> ( Model, Cmd msg )
init _ =
    ( { lines =
            [ LineSegment2d.from
                (Point2d.fromCoordinates ( 0, 0 ))
                (Point2d.fromCoordinates ( 0, 1920 ))
            , LineSegment2d.from
                (Point2d.fromCoordinates ( 0, 1920 ))
                (Point2d.fromCoordinates ( 1280, 1920 ))
            , LineSegment2d.from
                (Point2d.fromCoordinates ( 1280, 1920 ))
                (Point2d.fromCoordinates ( 1280, 0 ))
            ]
      , marble =
            { centerPoint = Point2d.fromCoordinates ( 640, 0 )
            , velocity = Vector2d.fromComponents ( 0, 3 )
            , radius = 80
            }
      }
    , Cmd.none
    )


type Msg
    = Tick Float
    | OnKeyDown String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick delta ->
            ( { model | marble = moveDown model.lines delta model.marble }
            , Cmd.none
            )

        OnKeyDown code ->
            case code of
                "KeyA" ->
                    ( { model | marble = moveLeft model.marble }
                    , Cmd.none
                    )

                _ ->
                    ( model, Cmd.none )


moveLeft : Marble -> Marble
moveLeft marble =
    { marble | velocity = Vector2d.sum marble.velocity (Vector2d.fromComponents ( -5, 0 )) }


moveDown : List LineSegment2d -> Float -> Marble -> Marble
moveDown lines delta marble =
    let
        --animation displacement (maximal possible step)
        displacement =
            Vector2d.scaleBy delta marble.velocity

        --displacement from center to radius
        boundary radius centerPoint =
            Point2d.translateBy (Vector2d.fromComponents ( 0, radius )) centerPoint

        newPoint =
            Point2d.translateBy displacement marble.centerPoint

        intersect line =
            LineSegment2d.intersectionPoint line <|
                LineSegment2d.from
                    (boundary marble.radius marble.centerPoint)
                    (boundary marble.radius newPoint)

        intersections =
            List.filterMap intersect lines
    in
    case List.head intersections of
        Nothing ->
            { marble | centerPoint = newPoint }

        Just intersectionPoint ->
            let
                translation =
                    Vector2d.from intersectionPoint marble.centerPoint
                        |> Vector2d.normalize
                        |> Vector2d.scaleBy marbleRadius
            in
            { marble | centerPoint = Point2d.translateBy translation intersectionPoint }


keyDownDecoder : Decoder Msg
keyDownDecoder =
    Decode.map OnKeyDown (Decode.field "code" Decode.string)


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Browser.Events.onAnimationFrameDelta Tick
        , Browser.Events.onKeyDown keyDownDecoder
        ]


view : Model -> Html msg
view model =
    Svg.svg
        [ Html.Attributes.attribute "viewBox" "0 0 1280 1920"

        --, Html.Attributes.style "width" "100%"
        , Html.Attributes.style "height" "540px"
        ]
        [ Svg.g []
            (List.map renderline model.lines)
        , Svg.circle
            [ Svg.Attributes.cx (String.fromFloat (Point2d.xCoordinate model.marble.centerPoint))
            , Svg.Attributes.cy (String.fromFloat (Point2d.yCoordinate model.marble.centerPoint))
            , Svg.Attributes.r (String.fromFloat model.marble.radius)
            ]
            []
        ]


renderline lineSegment =
    Svg.line
        [ Svg.Attributes.x1 (String.fromFloat (Point2d.xCoordinate (LineSegment2d.startPoint lineSegment)))
        , Svg.Attributes.y1 (String.fromFloat (Point2d.yCoordinate (LineSegment2d.startPoint lineSegment)))
        , Svg.Attributes.x2 (String.fromFloat (Point2d.xCoordinate (LineSegment2d.endPoint lineSegment)))
        , Svg.Attributes.y2 (String.fromFloat (Point2d.yCoordinate (LineSegment2d.endPoint lineSegment)))
        , Svg.Attributes.strokeWidth "2"
        , Svg.Attributes.stroke "black"
        ]
        []
