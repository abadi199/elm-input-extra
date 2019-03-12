module Input.BigNumber exposing (input, Options, defaultOptions)

{-| Big Number input.
This is a number input for big number values that can't be stored using `Int` and uses `String` instead.


# View

@docs input, Options, defaultOptions

-}

import Char
import Html exposing (Attribute, Html)
import Html.Attributes as Attributes exposing (style, type_, value)
import Html.Events exposing (keyCode, preventDefaultOn)
import Input.Decoder exposing (eventDecoder)
import Input.KeyCode exposing (allowedKeyCodes)
import Json.Decode as Json
import String


{-| Options of the input component.

  - `maxLength` is the maximum number of character allowed in this input. Set to `Nothing` for no limit.
  - `onInput` is the Msg tagger for the onInput event.
  - `hasFocus` is an optional Msg tagger for onFocus/onBlur event.

-}
type alias Options msg =
    { maxLength : Maybe Int
    , onInput : String -> msg
    , hasFocus : Maybe (Bool -> msg)
    }


{-| Default value for `Options`.
Params:

  - `onInput` (type: `String -> msg`) : The onInput Msg tagger

Value:

    { onInput = onInput
    , maxLength = Nothing
    , hasFocus = Nothing
    }

-}
defaultOptions : (String -> msg) -> Options msg
defaultOptions onInput =
    { onInput = onInput
    , maxLength = Nothing
    , hasFocus = Nothing
    }


{-| View function

Example:

    type Msg = InputUpdated String | FocusUpdated Bool

    Input.Number.input
        { onInput = InputUpdated
        , maxLength = Nothing
        , maxValue = 1000
        , minValue = 10
        , hasFocus = Just FocusUpdated
        }
        [ class "numberInput"
        ...
        ]
        model.currentValue

-}
input : Options msg -> List (Attribute msg) -> String -> Html msg
input options attributes currentValue =
    let
        toArray =
            \a -> (::) a []

        onFocusAttribute =
            options.hasFocus
                |> Maybe.map (\f -> f True)
                |> Maybe.map Html.Events.onFocus
                |> Maybe.map toArray
                |> Maybe.withDefault []

        onBlurAttribute =
            options.hasFocus
                |> Maybe.map (\f -> f False)
                |> Maybe.map Html.Events.onBlur
                |> Maybe.map toArray
                |> Maybe.withDefault []
    in
    Html.input
        (List.append attributes
            [ value currentValue
            , onKeyDown options currentValue
            , Html.Events.onInput options.onInput
            , onChange options
            , type_ "number"
            ]
            |> List.append onFocusAttribute
            |> List.append onBlurAttribute
        )
        []


filterNonDigit : String -> String
filterNonDigit value =
    value |> String.toList |> List.filter Char.isDigit |> String.fromList


onKeyDown : Options msg -> String -> Attribute msg
onKeyDown options currentValue =
    let
        newValue keyCode =
            keyCode
                |> Char.fromCode
                |> String.fromChar
                |> (++) currentValue

        isNumPad keyCode =
            keyCode
                >= 96
                && keyCode
                <= 105

        isNumber keyCode =
            keyCode
                >= 48
                && keyCode
                <= 57

        filterKey =
            \event ->
                if event.ctrlKey || event.altKey || event.metaKey then
                    ( options.onInput currentValue, False )

                else if event.shiftKey then
                    ( options.onInput currentValue, False )

                else if List.any ((==) event.keyCode) allowedKeyCodes then
                    ( options.onInput currentValue, False )

                else if
                    (isNumber event.keyCode || isNumPad event.keyCode)
                        && isValid (newValue event.keyCode) options
                then
                    ( options.onInput (newValue event.keyCode), False )

                else
                    ( options.onInput currentValue, True )

        decoder =
            eventDecoder
                |> Json.map filterKey
    in
    preventDefaultOn "keydown" decoder


isValid : String -> Options msg -> Bool
isValid newValue options =
    let
        updatedNumber =
            newValue
                |> String.toInt
    in
    not (exceedMaxLength options newValue)


onChange : Options msg -> Html.Attribute msg
onChange options =
    Html.Events.on "change" (Json.map options.onInput Html.Events.targetValue)


exceedMaxLength : Options msg -> String -> Bool
exceedMaxLength options value =
    options.maxLength
        |> Maybe.map (\maxLength -> maxLength >= String.length value)
        |> Maybe.map not
        |> Maybe.withDefault False
