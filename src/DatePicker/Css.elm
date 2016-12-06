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
        , padding (px 7)
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
                [ dayMixin ]
            , selector "td:hover" [ backgroundColor highlightedDay ]
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
            ]
        ]
    , (.) Footer
        [ textAlign center
        , backgroundColor lightGray
        , padding (px 8)
        ]
    ]


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
        [ padding2 (px 7) (px 8)
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
