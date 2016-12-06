module DatePicker.Formatter
    exposing
        ( titleFormatter
        , defaultFormatter
        , fullDateFormatter
        )

import Date exposing (Date)
import Date.Extra.Config.Config_en_us exposing (config)
import Date.Extra.Format


titleFormatter : Date -> String
titleFormatter =
    Date.Extra.Format.format config "%B %Y"


defaultFormatter : Date -> String
defaultFormatter =
    Date.Extra.Format.format config "%m/%d/%Y"


fullDateFormatter : Date -> String
fullDateFormatter =
    Date.Extra.Format.format config "%A, %B %d, %Y"
