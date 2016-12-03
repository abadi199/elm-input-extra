module DatePicker.Svg exposing (leftArrow, rightArrow)

import Svg exposing (Svg, svg, polygon)
import Svg.Attributes exposing (width, height, viewBox, points)


rightArrow : Svg msg
rightArrow =
    svg [ width "8", height "12", viewBox "0 0 15 15" ]
        [ polygon [ points "0 0, 0 20, 15 10" ] []
        ]


leftArrow : Svg msg
leftArrow =
    svg [ width "8", height "12", viewBox "0 0 15 15" ]
        [ polygon [ points "15 0, 15 20, 0 10" ] [] ]
