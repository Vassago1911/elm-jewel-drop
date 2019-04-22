module Main exposing (main)

import Html exposing (Html)
import Html.Attributes
import Svg
import Svg.Attributes
import Browser
import Browser.Events 

import Point2d exposing (Point2d)
import Vector2d 
import LineSegment2d exposing (LineSegment2d)

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
    , marble : Point2d
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
            Point2d.fromCoordinates ( 640, 0 )
      }
    , Cmd.none 
    )


type Msg 
    = Tick Float

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick delta -> 
            ( { model | marble = moveDown model.lines delta model.marble}
            , Cmd.none
            )

moveDown : List LineSegment2d -> Float -> Point2d -> Point2d
moveDown lines delta point =
    let 
        displacement =
            Vector2d.fromComponents ( 0, delta )

        newPoint =
            Point2d.translateBy displacement point

        intersect line =
            LineSegment2d.intersectionPoint line (LineSegment2d.from point newPoint)
        
        intersections = 
            List.filterMap intersect lines
    in
    case List.head intersections of 
        Nothing -> 
            newPoint

        Just intersectionPoint ->
            let
                translation =
                    Vector2d.from intersectionPoint point
                        |> Vector2d.normalize
                        |> Vector2d.scaleBy 20                    
            in            
            Point2d.translateBy translation intersectionPoint
              

subscriptions : Model -> Sub Msg
subscriptions model =
    Browser.Events.onAnimationFrameDelta Tick

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
            [ Svg.Attributes.cx (String.fromFloat (Point2d.xCoordinate model.marble))
            , Svg.Attributes.cy (String.fromFloat (Point2d.yCoordinate model.marble))
            , Svg.Attributes.r "20"
            ] [] 
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