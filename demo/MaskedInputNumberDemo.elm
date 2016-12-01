module MaskedInputNumberDemo exposing (main)

import Html exposing (Html, text, p, label, form, ul, li)
import Html.Attributes as Html exposing (style, for)
import MaskedInput.Number as MaskedNumber


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }


type alias Model =
    { value : Maybe Int
    , state : MaskedNumber.State
    , hasFocus : Bool
    }


init : ( Model, Cmd Msg )
init =
    ( { value = Nothing, hasFocus = False, state = MaskedNumber.initialState }
    , Cmd.none
    )


inputOptions : MaskedNumber.Options Msg
inputOptions =
    let
        defaultOptions =
            MaskedNumber.defaultOptions InputChanged InputStateChanged
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
                , MaskedNumber.input
                    inputOptions
                    [ Html.classList [ ( "focused", model.hasFocus ) ] ]
                    model.state
                    model.value
                ]
            ]
        , p []
            [ ul []
                [ li [] [ text "Pattern: ", text inputOptions.pattern ]
                , li [] [ text "Value: ", text <| toString model.value ]
                , li [] [ text "Has Focus: ", text <| toString model.hasFocus ]
                ]
            ]
        ]


type Msg
    = NoOp
    | InputChanged (Maybe Int)
    | FocusChanged Bool
    | InputStateChanged MaskedNumber.State


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        InputChanged value ->
            ( { model | value = value }, Cmd.none )

        FocusChanged bool ->
            ( { model | hasFocus = bool }, Cmd.none )

        InputStateChanged state ->
            ( { model | state = state }, Cmd.none )
