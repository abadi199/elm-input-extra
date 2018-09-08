module InputNumberDemo exposing (main)

import Browser
import Html exposing (Html, form, label, li, p, text, ul)
import Html.Attributes as Html exposing (for, style)
import Input.Number as Number


main : Program () Model Msg
main =
    Browser.sandbox
        { init = init
        , update = update
        , view = view
        }


type alias Model =
    { value : Maybe Int
    , valueString : String
    , hasFocus : Bool
    , hasFocusString : Bool
    }


init : Model
init =
    { value = Nothing, valueString = "", hasFocus = False, hasFocusString = False }


inputOptions : Number.Options Msg
inputOptions =
    let
        defaultOptions =
            Number.defaultOptions InputChanged
    in
    { defaultOptions
        | maxLength = Nothing
        , maxValue = Just 1000
        , minValue = Just 10
        , hasFocus = Just FocusChanged
    }


inputStringOptions : Number.StringOptions Msg
inputStringOptions =
    let
        defaultOptions =
            Number.defaultStringOptions InputStringChanged
    in
    { defaultOptions
        | maxLength = Just 4
        , maxValue = Just 1000
        , minValue = Just 10
        , hasFocus = Just FocusStringChanged
    }


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


view : Model -> Html Msg
view model =
    form []
        [ p []
            [ label []
                [ text "Number Input: "
                , Number.input
                    inputOptions
                    [ Html.classList [ ( "focused", model.hasFocus ) ] ]
                    model.value
                ]
            ]
        , p []
            [ ul []
                [ li [] [ text "Max Length: ", text <| Maybe.withDefault "No Limit" <| Maybe.map String.fromInt <| inputOptions.maxLength ]
                , li [] [ text "Max Value: ", text <| Maybe.withDefault "No Max" <| Maybe.map String.fromInt <| inputOptions.maxValue ]
                , li [] [ text "Min Value: ", text <| Maybe.withDefault "No Min" <| Maybe.map String.fromInt <| inputOptions.minValue ]
                , li [] [ text "Value: ", text <| Maybe.withDefault "NaN" <| Maybe.map String.fromInt <| model.value ]
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
                [ text "Number Input: "
                , Number.inputString
                    inputStringOptions
                    [ Html.classList [ ( "focused", model.hasFocusString ) ] ]
                    model.valueString
                ]
            ]
        , p []
            [ ul []
                [ li [] [ text "Max Length: ", text <| Maybe.withDefault "No Limit" <| Maybe.map String.fromInt <| inputOptions.maxLength ]
                , li [] [ text "Max Value: ", text <| Maybe.withDefault "No Max" <| Maybe.map String.fromInt <| inputOptions.maxValue ]
                , li [] [ text "Min Value: ", text <| Maybe.withDefault "No Min" <| Maybe.map String.fromInt <| inputOptions.minValue ]
                , li [] [ text "Value: ", text <| Maybe.withDefault "NaN" <| Maybe.map String.fromInt <| model.value ]
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
    | InputChanged (Maybe Int)
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
