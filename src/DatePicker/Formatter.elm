module DatePicker.Formatter
    exposing
        ( titleFormatter
        , defaultFormatter
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
