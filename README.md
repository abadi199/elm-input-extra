# Elm Input Extra

## Number Input

Html input component that only accept numeric values. This component implements [The Elm Architecture (TEA)](http://guide.elm-lang.org/architecture/index.html).

### Options
 * `id` is the id of the HTML element.
 * `maxLength` is the maximum number of character allowed in this input. Set to `Nothing` for no limit.
 * `maxValue` is the maximum number value allowed in this input. Set to `Nothing` for no limit.
 * `minValue` is the minimum number value allowed in this input. Set to `Nothing` for no limit.

## Text Input

Html input component with extra feature. This component implements [The Elm Architecture (TEA)](http://guide.elm-lang.org/architecture/index.html).

### Options
 * `id` is the id of the HTML element.
 * `maxLength` is the maximum number of character allowed in this input. Set to `Nothing` for no limit.



## Example
```elm
import Input.Number as Number
import Input.Text as Text
import Html exposing (Html, text)
import Html.Attributes exposing (style, for)
import Html.App as Html

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
        , maxLength = Just 4
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

```