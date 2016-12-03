module MultiSelect exposing (multiSelect, Item, Options, defaultOptions)

{-| MultiSelect

`<select>` element with multiple selection.
This will properly give you the selected values for `onChange` event since the core `onChange` on `select` doesn't.

Options
@docs Item, Options, defaultOptions

# View
@docs multiSelect
-}

import Html exposing (select, option, Html)
import Html.Attributes as Html
import Html.Events exposing (on, targetValue)
import Json.Decode as Decode


{-| Item is the individual content of the dropdown.

 * `value` is the item value or `id`
 * `text` is the display text of the multi-select item.
 * `enabled` is a flag to indicate whether the item is enabled or disabled.
-}
type alias Item =
    { value : String, text : String, enabled : Bool }


{-| Options for the dropdown.

 * `items` is content of the dropdown.
 * `onChange` is the message for when the selected value in the multi-select is changed.
-}
type alias Options msg =
    { items : List Item, onChange : List String -> msg }


{-| Default Options, will give you empty multi-select with no empty item
-}
defaultOptions : (List String -> msg) -> Options msg
defaultOptions onChange =
    { items = [], onChange = onChange }


{-| Html element.

Put this in your view's Html content.
Example:

    type Msg = MultiSelectChanged (List String)

    Html.div []
        [ multiSelect
            (defaultOptions MultiSelectChanged)
            [ class "my-multiSelect" ]
            model.selectedValues
        ]
-}
multiSelect : Options msg -> List (Html.Attribute msg) -> List String -> Html msg
multiSelect options attributes currentValue =
    let
        isSelected value =
            currentValue |> List.any ((==) value)

        toOption { value, text, enabled } =
            option
                [ Html.value value
                , Html.selected (isSelected value)
                , Html.disabled (not enabled)
                ]
                [ Html.text text ]
    in
        select
            (attributes ++ [ onChange options.onChange, Html.multiple True ])
            (List.map toOption options.items)


onChange : (List String -> msg) -> Html.Attribute msg
onChange tagger =
    on "change" (Decode.map tagger selectedOptionsDecoder)


selectedOptionsDecoder : Decode.Decoder (List String)
selectedOptionsDecoder =
    let
        filterSelected options =
            options
                |> List.filter .selected
                |> List.map .value
    in
        Decode.field "target" optionsDecoder
            |> Decode.map filterSelected


optionsDecoder : Decode.Decoder (List Option)
optionsDecoder =
    let
        loop idx xs =
            Decode.maybe (Decode.field (toString idx) optionDecoder)
                |> Decode.andThen
                    (Maybe.map (\x -> loop (idx + 1) (x :: xs))
                        >> Maybe.withDefault (Decode.succeed xs)
                    )
    in
        (Decode.field "options" <| loop 0 [])
            |> Decode.map List.reverse


type alias Option =
    { value : String, text : String, selected : Bool }


optionDecoder : Decode.Decoder Option
optionDecoder =
    Decode.map3 Option
        (Decode.field "value" Decode.string)
        (Decode.field "text" Decode.string)
        (Decode.field "selected" Decode.bool)
