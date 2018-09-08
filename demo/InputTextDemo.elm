module InputTextDemo exposing (main)

import Browser
import Html exposing (Html, form, label, li, p, text, ul)
import Html.Attributes as Html exposing (for, style)
import Input.Text as Text


main : Program () Model Msg
main =
    Browser.sandbox
        { init = init
        , update = update
        , view = view
        }


type alias Model =
    { value : String
    , hasFocus : Bool
    }


init : Model
init =
    { value = "", hasFocus = False }


inputOptions : Text.Options Msg
inputOptions =
    let
        defaultOptions =
            Text.defaultOptions InputChanged
    in
    { defaultOptions
        | maxLength = Just 5
        , hasFocus = Just FocusChanged
    }


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


view : Model -> Html Msg
view model =
    form []
        [ p []
            [ label []
                [ text "Text Input: "
                , Text.input
                    inputOptions
                    [ Html.classList [ ( "focused", model.hasFocus ) ] ]
                    model.value
                ]
            ]
        , p []
            [ ul []
                [ li [] [ text "Max Length: ", text <| Maybe.withDefault "No Limit" <| Maybe.map String.fromInt <| inputOptions.maxLength ]
                , li [] [ text "Value: ", text model.value ]
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
    | InputChanged String
    | FocusChanged Bool


update : Msg -> Model -> Model
update msg model =
    case msg of
        NoOp ->
            model

        InputChanged value ->
            { model | value = value }

        FocusChanged bool ->
            { model | hasFocus = bool }
