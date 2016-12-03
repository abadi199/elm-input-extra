module DatePickerDemo exposing (main)

import Html exposing (Html, text, p, label, form, ul, li)
import Html.Attributes as Html exposing (style, for)
import DatePicker
import Date exposing (Date)


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }


type alias Model =
    { value : Maybe Date
    , datePickerState : DatePicker.State
    }


init : ( Model, Cmd Msg )
init =
    ( { value = Nothing, datePickerState = DatePicker.initialState }
    , DatePicker.initialCmd DatePickerStateChanged DatePicker.initialState
    )


datePickerOptions : DatePicker.Options Msg
datePickerOptions =
    let
        defaultOptions =
            DatePicker.defaultOptions DateChanged DatePickerStateChanged
    in
        defaultOptions


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


view : Model -> Html Msg
view model =
    form []
        [ p []
            [ label []
                [ text "Date Picker : "
                , DatePicker.datePicker
                    datePickerOptions
                    []
                    model.datePickerState
                    model.value
                ]
            ]
        , p []
            [ ul []
                [ li [] [ text "Value: ", text <| toString model.value ]
                ]
            ]
        ]


type Msg
    = NoOp
    | DateChanged (Maybe Date)
    | DatePickerStateChanged DatePicker.State


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        DateChanged value ->
            ( { model | value = value }, Cmd.none )

        DatePickerStateChanged state ->
            ( { model | datePickerState = state }, Cmd.none )
