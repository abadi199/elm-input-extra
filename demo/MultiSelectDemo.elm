module MultiSelectDemo exposing (main)

import Browser
import Html exposing (Html, form, label, li, p, text, ul)
import Html.Attributes exposing (for, style)
import MultiSelect


main : Program () Model Msg
main =
    Browser.sandbox
        { init = init
        , update = update
        , view = view
        }


type alias Model =
    { selectedValue : List String
    }


init : Model
init =
    { selectedValue = [] }


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
                [ li [] [ text "Selected Values: ", text <| Debug.toString model.selectedValue ] ]
            ]
        ]


type Msg
    = NoOp
    | MultiSelectChanged (List String)


update : Msg -> Model -> Model
update msg model =
    case msg of
        NoOp ->
            model

        MultiSelectChanged selectedValue ->
            { model | selectedValue = selectedValue }
