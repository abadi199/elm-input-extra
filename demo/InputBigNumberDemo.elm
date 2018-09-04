module InputNumberDemo exposing (main)

import Browser
import Html exposing (Html, form, label, li, p, text, ul)
import Html.Attributes as Html exposing (for, style)
import Input.BigNumber as Number


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


inputOptions : Number.Options Msg
inputOptions =
    let
        defaultOptions =
            Number.defaultOptions InputChanged
    in
    { defaultOptions
        | maxLength = Just 20
        , hasFocus = Just FocusChanged
    }


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
                , li [] [ text "Value: ", text model.value ]
                , li [] [ text "Has Focus: ", text <| boolToString model.hasFocus ]
                ]
            ]
        ]


boolToString : Bool -> String
boolToString bool =
    if bool then
        "True"

    else
        "False"


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
