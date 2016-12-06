module DatePicker.SharedStyles
    exposing
        ( CssClasses(..)
        , datepickerNamespace
        )

import Html.CssHelpers


datepickerNamespace : Html.CssHelpers.Namespace String class id msg
datepickerNamespace =
    Html.CssHelpers.withNamespace "elm-input-datepicker"


type CssClasses
    = Calendar
    | DaysOfWeek
    | PreviousMonth
    | CurrentMonth
    | Header
    | NextMonth
    | Days
    | Title
    | ArrowLeft
    | ArrowRight
    | Dialog
    | DatePicker
    | Footer
