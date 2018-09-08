module InputFloatDemo exposing (main)

import Browser
import Html exposing (Html, form, label, li, p, text, ul)
import Html.Attributes as Html exposing (for, style)
import Input.Float as Float


main : Program () Model Msg
main =
    Browser.sandbox
        { init = init
        , update = update
        , view = view
        }


type alias Model =
    { value : Maybe Float
    , valueString : String
    , hasFocus : Bool
    , hasFocusString : Bool
    }


init : Model
init =
    { value = Nothing, valueString = "", hasFocus = False, hasFocusString = False }


inputOptions : Float.Options Msg
inputOptions =
    let
        defaultOptions =
            Float.defaultOptions InputChanged
    in
    { defaultOptions
        | maxValue = Just 1000
        , minValue = Just 10
        , hasFocus = Just FocusChanged
    }


inputStringOptions : Float.StringOptions Msg
inputStringOptions =
    let
        defaultOptions =
            Float.defaultStringOptions InputStringChanged
    in
    { defaultOptions
        | maxValue = Just 1000
        , minValue = Just 10
        , hasFocus = Just FocusStringChanged
    }


view : Model -> Html Msg
view model =
    form []
        [ p []
            [ label []
                [ text "Float Input: "
                , Float.input
                    inputOptions
                    [ Html.classList [ ( "focused", model.hasFocus ) ] ]
                    model.value
                ]
            ]
        , p []
            [ ul []
                [ li [] [ text "Max Value: ", text <| Maybe.withDefault "No Max" <| Maybe.map String.fromFloat <| inputOptions.maxValue ]
                , li [] [ text "Min Value: ", text <| Maybe.withDefault "No Min" <| Maybe.map String.fromFloat <| inputOptions.minValue ]
                , li [] [ text "Value: ", text <| Maybe.withDefault "NaN" <| Maybe.map String.fromFloat <| model.value ]
                , li []
                    [ text "Has Focus: "
                    , text <|
                        if model.hasFocus then
                            "True"

                        else
                            "False"
                    ]
                ]
            ]
        , p []
            [ label []
                [ text "Float Input: "
                , Float.inputString
                    inputStringOptions
                    [ Html.classList [ ( "focused", model.hasFocusString ) ] ]
                    model.valueString
                ]
            ]
        , p []
            [ ul []
                [ li [] [ text "Max Value: ", text <| Maybe.withDefault "No Max" <| Maybe.map String.fromFloat <| inputOptions.maxValue ]
                , li [] [ text "Min Value: ", text <| Maybe.withDefault "No Min" <| Maybe.map String.fromFloat <| inputOptions.minValue ]
                , li [] [ text "Value: ", text <| model.valueString ]
                , li []
                    [ text "Has Focus: "
                    , text <|
                        if model.hasFocus then
                            "True"

                        else
                            "False"
                    ]
                ]
            ]
        ]


type Msg
    = NoOp
    | InputChanged (Maybe Float)
    | InputStringChanged String
    | FocusChanged Bool
    | FocusStringChanged Bool


update : Msg -> Model -> Model
update msg model =
    case msg of
        NoOp ->
            model

        InputChanged value ->
            { model | value = value }

        FocusChanged bool ->
            { model | hasFocus = bool }

        InputStringChanged value ->
            { model | valueString = value }

        FocusStringChanged bool ->
            { model | hasFocusString = bool }
