module InputNumberDemo exposing (main)

import Html exposing (Html, text, p, label, form, ul, li)
import Html.Attributes as Html exposing (style, for)
import Input.BigNumber as Number


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }


type alias Model =
    { value : String
    , hasFocus : Bool
    }


init : ( Model, Cmd Msg )
init =
    ( { value = "", hasFocus = False }
    , Cmd.none
    )


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
                [ li [] [ text "Max Length: ", text <| Maybe.withDefault "No Limit" <| Maybe.map toString <| inputOptions.maxLength ]
                , li [] [ text "Value: ", text model.value ]
                , li [] [ text "Has Focus: ", text <| toString model.hasFocus ]
                ]
            ]
        ]


type Msg
    = NoOp
    | InputChanged String
    | FocusChanged Bool


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        InputChanged value ->
            ( { model | value = value }, Cmd.none )

        FocusChanged bool ->
            ( { model | hasFocus = bool }, Cmd.none )
