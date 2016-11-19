module Dropdown exposing (dropdown, Item, Options, defaultOptions)

{-| Dropdown

Options
@docs Item, Options, defaultOptions

# View
@docs dropdown
-}

import Html exposing (select, option, Html)
import Html.Attributes as Html
import Html.Events exposing (on, targetValue)
import Json.Decode as Decode


{-| Item is the individual content of the dropdown.

 * `value` is the item value or `id`
 * `text` is the display text of the dropdown item.
 * `enabled` is a flag to indicate whether the item is enabled or disabled.
-}
type alias Item =
    { value : String, text : String, enabled : Bool }


{-| Options for the dropdown.

 * `items` is content of the dropdown.
 * `emptyItem` is the item for when the nothing is selected. Set to `Nothing` for no empty item.
 * `onChange` is the message for when the selected value in the dropdown is changed.
-}
type alias Options msg =
    { items : List Item, emptyItem : Maybe Item, onChange : Maybe String -> msg }


{-| Default Options, will give you empty dropdown with no empty item
-}
defaultOptions : (Maybe String -> msg) -> Options msg
defaultOptions onChange =
    { items = [], emptyItem = Nothing, onChange = onChange }


{-| Html element.

Put this in your view's Html content.
Example:

    type Msg = DropdownChanged String

    Html.div []
        [ Dropdown.dropdown
            (Dropdown.defaultOptions DropdownChanged)
            [ class "my-dropdown" ]
            model.selectedDropdownValue
        ]
-}
dropdown : Options msg -> List (Html.Attribute msg) -> Maybe String -> Html msg
dropdown options attributes currentValue =
    let
        isSelected value =
            currentValue |> Maybe.map ((==) value) |> Maybe.withDefault False

        toOption { value, text, enabled } =
            option
                [ Html.value value
                , Html.selected (isSelected value)
                , Html.disabled (not enabled)
                ]
                [ Html.text text ]

        itemsWithEmptyItems =
            case options.emptyItem of
                Just emptyItem ->
                    emptyItem :: options.items

                Nothing ->
                    options.items
    in
        select
            (attributes ++ [ onChange options.emptyItem options.onChange ])
            (List.map toOption itemsWithEmptyItems)


onChange : Maybe Item -> (Maybe String -> msg) -> Html.Attribute msg
onChange emptyItem tagger =
    let
        textToMaybe string =
            if Maybe.map (.value >> (==) string) emptyItem |> Maybe.withDefault False then
                Nothing
            else
                Just string
    in
        on "change" (Decode.map (textToMaybe >> tagger) targetValue)
