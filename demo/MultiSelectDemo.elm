module MultiSelectDemo exposing (main)

import Html exposing (Html, text, p, label, form, ul, li)
import Html.Attributes exposing (style, for)
import MultiSelect


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }


type alias Model =
    { selectedValue : List String
    }


init : ( Model, Cmd Msg )
init =
    ( { selectedValue = []
      }
    , Cmd.none
    )


multiSelectOptions : MultiSelect.Options Msg
multiSelectOptions =
    let
        defaultOptions =
            MultiSelect.defaultOptions MultiSelectChanged
    in
        { defaultOptions
            | items =
                [ { value = "1", text = "One", enabled = True }
                , { value = "2", text = "Two", enabled = True }
                , { value = "3", text = "Three", enabled = True }
                , { value = "4", text = "Four", enabled = True }
                ]
        }


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


view : Model -> Html Msg
view model =
    form []
        [ p []
            [ label []
                [ text "MultiSelect: "
                , MultiSelect.multiSelect
                    multiSelectOptions
                    []
                    model.selectedValue
                ]
            ]
        , p []
            [ ul []
                [ li [] [ text "Selected Values: ", text <| toString model.selectedValue ] ]
            ]
        ]


type Msg
    = NoOp
    | MultiSelectChanged (List String)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        MultiSelectChanged selectedValue ->
            ( { model | selectedValue = selectedValue }, Cmd.none )
