module DropdownDemo exposing (main)

import Browser
import Dropdown
import Html exposing (Html, form, label, li, p, text, ul)
import Html.Attributes exposing (for, style)


main : Program () Model Msg
main =
    Browser.sandbox
        { init = init
        , update = update
        , view = view
        }


type alias Model =
    { selectedValue : Maybe String
    }


init : Model
init =
    { selectedValue = Nothing
    }


dropdownOptions : Dropdown.Options Msg
dropdownOptions =
    let
        defaultOptions =
            Dropdown.defaultOptions DropdownChanged
    in
    { defaultOptions
        | items =
            [ { value = "1", text = "One", enabled = True }
            , { value = "2", text = "Two", enabled = True }
            ]
        , emptyItem = Just { value = "0", text = "[Please Select]", enabled = True }
    }


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


view : Model -> Html Msg
view model =
    form []
        [ p []
            [ label []
                [ text "Dropdown: "
                , Dropdown.dropdown
                    dropdownOptions
                    []
                    model.selectedValue
                ]
            ]
        , p []
            [ ul []
                [ li [] [ text "Selected Value: ", text <| Maybe.withDefault "Not Selected" model.selectedValue ] ]
            ]
        ]


type Msg
    = NoOp
    | DropdownChanged (Maybe String)


update : Msg -> Model -> Model
update msg model =
    case msg of
        NoOp ->
            model

        DropdownChanged selectedValue ->
            { model | selectedValue = selectedValue }
