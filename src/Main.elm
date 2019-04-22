module Main exposing (main)

import Html exposing (Html)
import Html.Attributes
import Svg
import Svg.Attributes

main =
    view init
    
type alias Model = 
    { lines : List Line
    , marble : Point
    }

type Line
    = Line Point Point

type alias Point =
    { x : Float
    , y : Float
    }

init : Model
init =
    { lines = 
        [ Line { x = 0, y = 0 } { x = 0, y = 1920 }
        , Line { x = 0, y = 1920 } { x = 1280, y = 1920 }
        , Line { x = 1280, y = 1920 } { x = 1280, y = 0 }
        ]
    , marble = { x = 640, y = 0 }
    }

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
            [ Svg.Attributes.cx (String.fromFloat model.marble.x)
            , Svg.Attributes.cy (String.fromFloat model.marble.y)
            , Svg.Attributes.r "20"
            ] [] 
        ]
          
        

renderline (Line start end) =
    Svg.line
        [ Svg.Attributes.x1 (String.fromFloat start.x)
        , Svg.Attributes.y1 (String.fromFloat start.y)
        , Svg.Attributes.x2 (String.fromFloat end.x)
        , Svg.Attributes.y2 (String.fromFloat end.y)
        , Svg.Attributes.strokeWidth "2"
        , Svg.Attributes.stroke "black"
        ]
        []