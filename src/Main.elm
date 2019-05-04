module Main exposing (main)

import Axis2d exposing (Axis2d)
import Browser
import Browser.Events exposing (Visibility(..))
import Html exposing (Html)
import Html.Attributes
import Json.Decode as Decode exposing (Decoder)
import LineSegment2d exposing (LineSegment2d)
import Point2d exposing (Point2d)
import Svg exposing (Svg)
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
    , marbles : List Marble
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
      , marble = initialMarble
      , marbles = []
      , paused = False
      , movement = Stay
      }
    , Cmd.none
    )


initialMarble : Marble
initialMarble =
    { centerPoint = Point2d.fromCoordinates ( 640, 0 )
    , velocity = Vector2d.fromComponents ( 0, 1 )
    , radius = 80
    }


type Msg
    = Tick Float
    | OnKeyDown String
    | OnKeyUp String
    | VisibilityChanged Visibility


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick delta ->
            let
                ( steppedMarbles, maybeSteppedMarble ) =
                    model.marble
                        |> stepUserMovement delta model.movement
                        |> stepMarbleVelocity delta
                        |> handleUserCollisions model.lines model.marbles
            in
            ( case maybeSteppedMarble of
                Nothing ->
                    { model
                        | marble = initialMarble
                        , marbles = steppedMarbles
                    }

                Just steppedMarble ->
                    if Vector2d.yComponent steppedMarble.velocity == 0 then
                        { model
                            | marble = initialMarble
                            , marbles = steppedMarble :: steppedMarbles
                        }

                    else
                        { model
                            | marble = steppedMarble
                            , marbles = steppedMarbles
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


axisFromLineSegment : LineSegment2d -> Maybe Axis2d
axisFromLineSegment lineSegment =
    case LineSegment2d.direction lineSegment of
        Nothing ->
            Nothing

        Just direction ->
            Just (Axis2d.through (LineSegment2d.startPoint lineSegment) direction)


distancePointAxis : Point2d -> Axis2d -> Vector2d
distancePointAxis point axis =
    let
        projectedPoint =
            point
                |> Point2d.projectOnto axis
    in
    Vector2d.from projectedPoint point


handleUserCollision : LineSegment2d -> Marble -> Marble
handleUserCollision lineSegment candidate =
    case axisFromLineSegment lineSegment of
        Nothing ->
            candidate

        Just axis ->
            let
                distanceVector =
                    distancePointAxis candidate.centerPoint axis

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
                { candidate
                    | centerPoint = Point2d.translateBy translationVector candidate.centerPoint
                    , velocity = Vector2d.projectOnto axis candidate.velocity
                }

            else
                candidate


handleUserMarbleCollision : List Marble -> Marble -> ( List Marble, Maybe Marble )
handleUserMarbleCollision marbles marble =
    stepUser marbles marble


collides : Marble -> Marble -> Bool
collides otherMarble marble =
    let
        distance =
            Vector2d.from marble.centerPoint otherMarble.centerPoint
                |> Vector2d.length
    in
    distance <= marble.radius + otherMarble.radius


splitMarbles : List Marble -> Marble -> ( List Marble, List Marble )
splitMarbles marbles marble =
    List.partition (collides marble) marbles


step : List Marble -> Marble -> List Marble
step marbles marble =
    let
        ( collidingMarbles, nonCollidingMarbles ) =
            splitMarbles marbles marble
    in
    if List.isEmpty collidingMarbles then
        nonCollidingMarbles ++ [ marble ]

    else
        let
            ( newCollidingMarbles, newMarble ) =
                makeCollision collidingMarbles marble
        in
        case newCollidingMarbles ++ nonCollidingMarbles of
            [] ->
                [ newMarble ]

            nextMarble :: rest ->
                step (rest ++ [ newMarble ]) nextMarble


makeCollision : List Marble -> Marble -> ( List Marble, Marble )
makeCollision passiveMarbles activeMarble =
    let
        velocityShare =
            Vector2d.scaleBy (1 / toFloat (List.length passiveMarbles)) activeMarble.velocity

        newPassiveMarbles =
            List.map addVelocityShare passiveMarbles

        addVelocityShare passiveMarble =
            case axisFromLineSegment (LineSegment2d.from passiveMarble.centerPoint activeMarble.centerPoint) of
                Nothing ->
                    passiveMarble

                Just axis ->
                    { passiveMarble | velocity = Vector2d.sum passiveMarble.velocity (Vector2d.projectOnto axis velocityShare) }
    in
    ( newPassiveMarbles, { activeMarble | velocity = Vector2d.zero } )


stepUser : List Marble -> Marble -> ( List Marble, Maybe Marble )
stepUser marbles marble =
    let
        ( collidingMarbles, nonCollidingMarbles ) =
            splitMarbles marbles marble
    in
    if List.isEmpty collidingMarbles then
        ( marbles, Just marble )

    else
        let
            ( newCollidingMarbles, newMarble ) =
                makeCollision collidingMarbles marble
        in
        case newCollidingMarbles ++ nonCollidingMarbles of
            [] ->
                ( [ newMarble ], Nothing )

            nextMarble :: rest ->
                ( step (rest ++ [ newMarble ]) nextMarble, Nothing )


handleUserCollisions : List LineSegment2d -> List Marble -> Marble -> ( List Marble, Maybe Marble )
handleUserCollisions lines marbles marble =
    List.foldl handleUserCollision marble lines
        |> handleUserMarbleCollision marbles


moveLeft : Marble -> Marble
moveLeft marble =
    { marble | velocity = Vector2d.sum marble.velocity (Vector2d.fromComponents ( -5, 0 )) }


stepUserMovement : Float -> UserMovement -> Marble -> Marble
stepUserMovement delta movement marble =
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


stepMarbleVelocity : Float -> Marble -> Marble
stepMarbleVelocity delta marble =
    let
        --animation displacement (maximal possible step)
        displacement =
            Vector2d.scaleBy delta marble.velocity

        newPoint =
            Point2d.translateBy displacement marble.centerPoint
    in
    { marble | centerPoint = newPoint }


stepGravity : Float -> Marble -> Marble
stepGravity delta marble =
    let
        gravity =
            Vector2d.fromComponents ( 0, 0.0005 )

        --animation displacement (maximal possible step)
        displacement =
            Vector2d.scaleBy delta gravity
    in
    { marble | velocity = Vector2d.sum displacement marble.velocity }


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
            (List.map renderLine model.lines)
        , renderMarble model.marble
        , Svg.g
            []
            (List.map renderMarble model.marbles)
        ]


renderLine : LineSegment2d -> Svg msg
renderLine lineSegment =
    Svg.line
        [ Svg.Attributes.x1 (String.fromFloat (Point2d.xCoordinate (LineSegment2d.startPoint lineSegment)))
        , Svg.Attributes.y1 (String.fromFloat (Point2d.yCoordinate (LineSegment2d.startPoint lineSegment)))
        , Svg.Attributes.x2 (String.fromFloat (Point2d.xCoordinate (LineSegment2d.endPoint lineSegment)))
        , Svg.Attributes.y2 (String.fromFloat (Point2d.yCoordinate (LineSegment2d.endPoint lineSegment)))
        , Svg.Attributes.strokeWidth "2"
        , Svg.Attributes.stroke "black"
        ]
        []


renderMarble : Marble -> Svg msg
renderMarble marble =
    Svg.circle
        [ Svg.Attributes.cx (String.fromFloat (Point2d.xCoordinate marble.centerPoint))
        , Svg.Attributes.cy (String.fromFloat (Point2d.yCoordinate marble.centerPoint))
        , Svg.Attributes.r (String.fromFloat marble.radius)
        ]
        []
