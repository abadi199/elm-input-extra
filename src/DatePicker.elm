module DatePicker exposing (Options, datePicker, defaultOptions, State, initialState)

{-| DatePicker

# View
@docs datePicker, Options, defaultOptions

# Internal State
@docs State
-}

import Date exposing (Date)
import Html exposing (Html, input, div, span, text, button)
import Html.Events exposing (onFocus, onBlur)
import Html.Attributes exposing (class, type_)


type alias Options msg =
    { onChange : Maybe Date -> msg
    , toMsg : State -> msg
    }


defaultOptions : (Maybe Date -> msg) -> (State -> msg) -> Options msg
defaultOptions onChange toMsg =
    { onChange = onChange
    , toMsg = toMsg
    }


type State
    = State StateValue


type alias StateValue =
    { inputFocused : Bool, dialogFocused : Bool }


initialState : State
initialState =
    State { inputFocused = False, dialogFocused = False }


datePicker : Options msg -> List (Html.Attribute msg) -> State -> Maybe Date -> Html msg
datePicker options attributes state currentDate =
    let
        stateValue =
            case state of
                State stateValue ->
                    stateValue

        datePickerAttributes =
            attributes
                ++ [ onFocus <| options.toMsg <| State { stateValue | inputFocused = True }
                   , onBlur <| options.toMsg <| State { stateValue | inputFocused = True }
                   ]
    in
        div [ class "elm-datepicker" ]
            [ input datePickerAttributes []
            , if stateValue.inputFocused || stateValue.dialogFocused then
                datePickerDialog options stateValue currentDate
              else
                text ""
            ]


datePickerDialog : Options msg -> StateValue -> Maybe Date -> Html msg
datePickerDialog options stateValue currentDate =
    div
        [ class "elm-datepicker--dialog"
        , onFocus <| options.toMsg <| State { stateValue | dialogFocused = True }
        , onBlur <| options.toMsg <| State { stateValue | dialogFocused = False }
        ]
        [ span [ class "elm-datepicker--arrow-left" ]
            [ button [ type_ "button" ] [ text "<<" ]
            ]
        , span [ class "elm-datepicker--title" ]
            [ button [ type_ "button" ] [ text "November 2011" ]
            ]
        , span [ class "elm-datepicker--arrow-right" ]
            [ button [ type_ "button" ] [ text ">>" ]
            ]
        ]
