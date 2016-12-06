module DatePicker.Css exposing (css)

import Css exposing (..)
import Css.Elements exposing (..)
import Css.Namespace exposing (namespace)
import DatePicker.SharedStyles exposing (CssClasses(..), datepickerNamespace)


css : Css.Stylesheet
css =
    (Css.stylesheet << namespace datepickerNamespace.name)
        [ (.) DatePicker
            [ position relative ]
        , (.) Dialog
            [ fontFamilies [ "Arial", "Helvetica", "sans-serif" ]
            , fontSize (px 14)
            , borderBoxMixin
            , position absolute
            , border3 (px 1) solid (darkGray)
            , boxShadow4 (px 0) (px 5) (px 10) (rgba 0 0 0 0.2)
            , children dialogCss
            ]
        ]


dialogCss : List Css.Snippet
dialogCss =
    [ (.) Header
        [ borderBoxMixin
        , backgroundColor (lightGray)
        , padding2 (px 10) (px 7)
        , position relative
        , children
            [ (.) ArrowLeft
                [ arrowMixin
                , left (px 0)
                ]
            , (.) ArrowRight
                [ arrowMixin
                , right (px 0)
                ]
            , (.) Title
                [ borderBoxMixin
                , display inlineBlock
                , width (pct 100)
                , textAlign center
                ]
            ]
        ]
    , (.) Calendar
        [ backgroundColor (hex "#ffffff")
        , property "border-spacing" "0"
        , property "border-width" "0"
        , property "table-layout" "fixed"
        , descendants
            [ thead
                []
            , td
                [ dayMixin
                , hover
                    [ backgroundColor highlightedDay
                    , highlightMixin
                    ]
                ]
            , th
                [ dayMixin
                , backgroundColor (lightGray)
                , fontWeight normal
                , borderBottom3 (px 1) solid (darkGray)
                ]
            , (.) PreviousMonth
                [ color fadeText ]
            , (.) NextMonth
                [ color fadeText
                ]
            , (.) SelectedDate
                [ property "box-shadow" "inset 0 0 10px 3px #3276b1"
                , backgroundColor selectedDate
                , color (hex "#fff")
                , highlightMixin
                ]
            , (.) Today
                [ property "box-shadow" "inset 0 0 7px 0 #76abd9"
                , highlightMixin
                , hover
                    [ backgroundColor highlightSelectedDay ]
                ]
            ]
        ]
    , (.) Footer
        [ textAlign center
        , backgroundColor lightGray
        , padding2 (px 10) (px 7)
        ]
    ]


highlightSelectedDay : Css.Color
highlightSelectedDay =
    hex "#d5e5f3"


selectedDate : Css.Color
selectedDate =
    hex "#428bca"


fadeText : Css.Color
fadeText =
    hex "#a1a1a1"


lightGray : Css.Color
lightGray =
    hex "#f5f5f5"


darkGray : Css.Color
darkGray =
    hex "#ccc"


highlightedDay : Css.Color
highlightedDay =
    hex "#ebebeb"


dayMixin : Css.Mixin
dayMixin =
    mixin
        [ padding4 (px 7) (px 7) (px 7) (px 9)
        , textAlign right
        , border (px 0)
        , cursor pointer
        ]


arrowMixin : Css.Mixin
arrowMixin =
    mixin
        [ borderBoxMixin
        , textAlign center
        , transform (scale 0.8)
        , position absolute
        , padding2 (px 0) (px 5)
        , cursor pointer
        ]


borderBoxMixin : Css.Mixin
borderBoxMixin =
    mixin [ boxSizing borderBox ]


highlightMixin : Css.Mixin
highlightMixin =
    mixin [ borderRadius (px 4) ]
