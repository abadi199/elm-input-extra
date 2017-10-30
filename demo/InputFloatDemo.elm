module InputFloatDemo exposing (main)

import Html exposing (Html, form, label, li, p, text, ul)
import Html.Attributes as Html exposing (for, style)
import Input.Float as Float


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }


type alias Model =
    { value : Maybe Float
    , valueString : String
    , hasFocus : Bool
    , hasFocusString : Bool
    }


init : ( Model, Cmd Msg )
init =
    ( { value = Nothing, valueString = "", hasFocus = False, hasFocusString = False }
    , Cmd.none
    )


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


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


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
                [ li [] [ text "Max Value: ", text <| Maybe.withDefault "No Max" <| Maybe.map toString <| inputOptions.maxValue ]
                , li [] [ text "Min Value: ", text <| Maybe.withDefault "No Min" <| Maybe.map toString <| inputOptions.minValue ]
                , li [] [ text "Value: ", text <| Maybe.withDefault "NaN" <| Maybe.map toString <| model.value ]
                , li [] [ text "Has Focus: ", text <| toString model.hasFocus ]
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
                [ li [] [ text "Max Value: ", text <| Maybe.withDefault "No Max" <| Maybe.map toString <| inputOptions.maxValue ]
                , li [] [ text "Min Value: ", text <| Maybe.withDefault "No Min" <| Maybe.map toString <| inputOptions.minValue ]
                , li [] [ text "Value: ", text <| Maybe.withDefault "NaN" <| Maybe.map toString <| model.value ]
                , li [] [ text "Has Focus: ", text <| toString model.hasFocus ]
                ]
            ]
        ]


type Msg
    = NoOp
    | InputChanged (Maybe Float)
    | InputStringChanged String
    | FocusChanged Bool
    | FocusStringChanged Bool


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        InputChanged value ->
            ( { model | value = value }, Cmd.none )

        FocusChanged bool ->
            ( { model | hasFocus = bool }, Cmd.none )

        InputStringChanged value ->
            ( { model | valueString = value }, Cmd.none )

        FocusStringChanged bool ->
            ( { model | hasFocusString = bool }, Cmd.none )
