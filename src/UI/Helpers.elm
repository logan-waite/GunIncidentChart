module UI.Helpers exposing (..)

import Element exposing (Attribute, Color)
import Element.Border as Border


border : Int -> Attribute msg -> Color -> List (Attribute msg)
border width style color =
    [ Border.width width
    , Border.color color
    , style
    ]


borderRight : Int -> Attribute msg -> Color -> List (Attribute msg)
borderRight width style color =
    [ Border.widthEach
        { bottom = 0
        , left = 0
        , right = width
        , top = 0
        }
    , Border.color color
    , style
    ]
