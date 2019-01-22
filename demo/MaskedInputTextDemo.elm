module MaskedInputTextDemo exposing (main)

import Browser
import Html exposing (Html, form, label, li, p, text, ul)
import Html.Attributes as Html exposing (for, style)
import MaskedInput.Text as MaskedText


main : Program () Model Msg
main =
    Browser.sandbox
        { init = init
        , update = update
        , view = view
        }


type alias Model =
    { value : String
    , state : MaskedText.State
    , hasFocus : Bool
    }


init : Model
init =
    { value = "", hasFocus = False, state = MaskedText.initialState }


inputOptions : MaskedText.Options Msg
inputOptions =
    let
        defaultOptions =
            MaskedText.defaultOptions InputChanged InputStateChanged
    in
    { defaultOptions
        | pattern = "(###) ###-####"
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
                [ text "Masked Input: "
                , MaskedText.input
                    inputOptions
                    [ Html.classList [ ( "focused", model.hasFocus ) ] ]
                    model.state
                    model.value
                ]
            ]
        , p []
            [ ul []
                [ li [] [ text "Pattern: ", text inputOptions.pattern ]
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
    | InputStateChanged MaskedText.State


update : Msg -> Model -> Model
update msg model =
    case msg of
        NoOp ->
            model

        InputChanged value ->
            { model | value = value }

        FocusChanged bool ->
            { model | hasFocus = bool }

        InputStateChanged state ->
            { model | state = state }
