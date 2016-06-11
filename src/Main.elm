module Main exposing (main)

import Input.Number as Number
import Input.Text as Text
import Html exposing (Html, text)
import Html.Attributes exposing (style, for)
import Html.App as Html


main : Program Never
main =
    Html.beginnerProgram
        { model = init
        , update = update
        , view = view
        }


type alias Model =
    { numberModel : Number.Model
    , numberOptions : Number.Options
    , textModel : Text.Model
    , textOptions : Text.Options
    }


init : Model
init =
    { numberModel = Number.init
    , numberOptions =
        { id = "NumberInput"
        , maxLength = Nothing
        , maxValue = Nothing
        , minValue = Nothing
        }
    , textModel = Text.init
    , textOptions =
        { id = "TextInput"
        , maxLength = Just 4
        }
    }


view : Model -> Html Msg
view model =
    Html.form []
        [ Html.p []
            [ Html.label [ for model.numberOptions.id ] [ text "Number Input" ]
            , Number.input model.numberOptions
                [ style
                    [ ( "border", "1px solid #ccc" )
                    , ( "padding", ".5rem" )
                    , ( "box-shadow", "inset 0 1px 1px rgba(0,0,0,.075);" )
                    ]
                ]
                model.numberModel
                |> Html.map UpdateNumber
            , Html.text ("value: " ++ model.numberModel.value)
            ]
        , Html.p []
            [ Html.label [ for model.textOptions.id ] [ text "Text Input" ]
            , Text.input model.textOptions
                [ style
                    [ ( "border", "1px solid #ccc" )
                    , ( "padding", ".5rem" )
                    , ( "box-shadow", "inset 0 1px 1px rgba(0,0,0,.075);" )
                    ]
                ]
                model.textModel
                |> Html.map UpdateText
            , Html.text ("value: " ++ model.textModel.value)
            ]
        ]


type Msg
    = NoOp
    | UpdateNumber Number.Msg
    | UpdateText Text.Msg


update : Msg -> Model -> Model
update msg model =
    case msg of
        NoOp ->
            model

        UpdateNumber numberMsg ->
            { model | numberModel = Number.update numberMsg model.numberModel }

        UpdateText textMsg ->
            { model | textModel = Text.update textMsg model.textModel }
