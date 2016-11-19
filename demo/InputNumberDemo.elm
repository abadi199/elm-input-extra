module InputNumberDemo exposing (main)

import Html exposing (Html, text, p, label, form)
import Html.Attributes exposing (style, for)
import Input.Number as Number


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
    , hasFocus : Bool
    }


init : ( Model, Cmd Msg )
init =
    ( { value = Nothing, hasFocus = False }
    , Cmd.none
    )


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
                    [ style
                        [ ( "border", "1px solid #ccc" )
                        , ( "padding", ".5rem" )
                        , ( "box-shadow", "inset 0 1px 1px rgba(0,0,0,.075);" )
                        , ( "background-color"
                          , if model.hasFocus then
                                "#ff0"
                            else
                                "#fff"
                          )
                        ]
                    ]
                    model.value
                ]
            ]
        , p [] [ text "Max Length: ", text <| Maybe.withDefault "No Limit" <| Maybe.map toString <| inputOptions.maxLength ]
        , p [] [ text "Max Value: ", text <| Maybe.withDefault "No Max" <| Maybe.map toString <| inputOptions.maxValue ]
        , p [] [ text "Min Value: ", text <| Maybe.withDefault "No Min" <| Maybe.map toString <| inputOptions.minValue ]
        , p [] [ text "Value: ", text <| Maybe.withDefault "NaN" <| Maybe.map toString <| model.value ]
        , p [] [ text "Has Focus: ", text <| toString model.hasFocus ]
        ]


type Msg
    = NoOp
    | InputChanged (Maybe Int)
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
