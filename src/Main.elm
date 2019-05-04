module Main exposing (main)

import Axis2d
import Browser
import Browser.Events exposing (Visibility(..))
import Html exposing (Html)
import Html.Attributes
import Json.Decode as Decode exposing (Decoder)
import LineSegment2d exposing (LineSegment2d)
import Point2d exposing (Point2d)
import Svg
import Svg.Attributes
import Vector2d exposing (Vector2d)


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
    , paused : Bool
    , movement : UserMovement
    }


type alias Marble =
    { centerPoint : Point2d
    , velocity : Vector2d
    , radius : Float
    }


type UserMovement
    = Left
    | Right
    | Stay


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
            , velocity = Vector2d.fromComponents ( 0, 1 )
            , radius = 80
            }
      , paused = False
      , movement = Stay
      }
    , Cmd.none
    )


type Msg
    = Tick Float
    | OnKeyDown String
    | OnKeyUp String
    | VisibilityChanged Visibility


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick delta ->
            ( { model
                | marble =
                    model.marble
                        |> moveDown delta
                        |> moveUser delta model.movement
                        |> handleCollisions model.lines
              }
            , Cmd.none
            )

        OnKeyDown code ->
            case code of
                "KeyA" ->
                    ( { model | movement = Left }
                    , Cmd.none
                    )

                "KeyD" ->
                    ( { model | movement = Right }
                    , Cmd.none
                    )

                _ ->
                    ( model, Cmd.none )

        OnKeyUp code ->
            ( { model | movement = Stay }
            , Cmd.none
            )

        VisibilityChanged visibility ->
            case visibility of
                Visible ->
                    ( { model | paused = False }, Cmd.none )

                Hidden ->
                    ( { model
                        | paused = True
                        , movement = Stay
                      }
                    , Cmd.none
                    )


distancePointLine : Point2d -> LineSegment2d -> Maybe Vector2d
distancePointLine point line =
    case LineSegment2d.direction line of
        Nothing ->
            Nothing

        Just direction ->
            let
                axis =
                    Axis2d.through (LineSegment2d.startPoint line) direction

                projectedPoint =
                    point
                        |> Point2d.projectOnto axis
            in
            Just (Vector2d.from projectedPoint point)


handleCollision : LineSegment2d -> Marble -> Marble
handleCollision line candidate =
    case distancePointLine candidate.centerPoint line of
        Nothing ->
            candidate

        Just distanceVector ->
            let
                distance =
                    Vector2d.length distanceVector
            in
            if distance <= candidate.radius then
                let
                    translationVector =
                        distanceVector
                            |> Vector2d.normalize
                            |> Vector2d.scaleBy (candidate.radius - distance)
                in
                { candidate | centerPoint = Point2d.translateBy translationVector candidate.centerPoint }

            else
                candidate


handleCollisions : List LineSegment2d -> Marble -> Marble
handleCollisions lines marble =
    List.foldl handleCollision marble lines


moveLeft : Marble -> Marble
moveLeft marble =
    { marble | velocity = Vector2d.sum marble.velocity (Vector2d.fromComponents ( -5, 0 )) }


moveUser : Float -> UserMovement -> Marble -> Marble
moveUser delta movement marble =
    let
        translation =
            case movement of
                Left ->
                    Vector2d.fromComponents ( -delta, 0 )

                Right ->
                    Vector2d.fromComponents ( delta, 0 )

                Stay ->
                    Vector2d.zero
    in
    { marble | centerPoint = Point2d.translateBy translation marble.centerPoint }


moveDown : Float -> Marble -> Marble
moveDown delta marble =
    let
        --animation displacement (maximal possible step)
        displacement =
            Vector2d.scaleBy delta marble.velocity

        newPoint =
            Point2d.translateBy displacement marble.centerPoint
    in
    { marble | centerPoint = newPoint }


keyCodeDecoder : (String -> msg) -> Decoder msg
keyCodeDecoder toMsg =
    Decode.map toMsg (Decode.field "code" Decode.string)


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ if model.paused then
            Sub.none

          else
            Browser.Events.onAnimationFrameDelta Tick
        , Browser.Events.onKeyDown (keyCodeDecoder OnKeyDown)
        , Browser.Events.onKeyUp (keyCodeDecoder OnKeyUp)
        , Browser.Events.onVisibilityChange VisibilityChanged
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
