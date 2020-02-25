module Input.Number exposing
    ( StringOptions, Options, defaultStringOptions, defaultOptions
    , input, inputString
    )

{-| Number input


# Options

@docs StringOptions, Options, defaultStringOptions, defaultOptions


# View

@docs input, inputString

-}

import Char
import Html exposing (Attribute, Html)
import Html.Attributes as Attributes exposing (max, min, style, type_, value)
import Html.Events exposing (keyCode, preventDefaultOn)
import Input.Decoder exposing (eventDecoder)
import Input.KeyCode exposing (allowedKeyCodes)
import Json.Decode as Json
import Regex
import String


type alias GenericOptions options =
    { options
        | maxLength : Maybe Int
        , maxValue : Maybe Int
        , minValue : Maybe Int
    }


{-| Options of the input component.

  - `maxLength` is the maximum number of character allowed in this input. Set to `Nothing` for no limit.
  - `maxValue` is the maximum number value allowed in this input. Set to `Nothing` for no limit.
  - `minValue` is the minimum number value allowed in this input. Set to `Nothing` for no limit.
  - `onInput` is the Msg tagger for the onInput event.
  - `hasFocus` is an optional Msg tagger for onFocus/onBlur event.

-}
type alias Options msg =
    { maxLength : Maybe Int
    , maxValue : Maybe Int
    , minValue : Maybe Int
    , onInput : Maybe Int -> msg
    , hasFocus : Maybe (Bool -> msg)
    }


{-| Options of the input component with `String` value.

  - `maxLength` is the maximum number of character allowed in this input. Set to `Nothing` for no limit.
  - `maxValue` is the maximum number value allowed in this input. Set to `Nothing` for no limit.
  - `minValue` is the minimum number value allowed in this input. Set to `Nothing` for no limit.
  - `onInput` is the Msg tagger for the onInput event.
  - `hasFocus` is an optional Msg tagger for onFocus/onBlur event.

-}
type alias StringOptions msg =
    { maxLength : Maybe Int
    , maxValue : Maybe Int
    , minValue : Maybe Int
    , onInput : String -> msg
    , hasFocus : Maybe (Bool -> msg)
    }


{-| Default value for `Options`.
Params:

  - `onInput` (type: `Maybe Int -> msg`) : The onInput Msg tagger

Value:

    { onInput = onInput
    , maxLength = Nothing
    , maxValue = Nothing
    , minValue = Nothing
    , hasFocus = Nothing
    }

-}
defaultOptions : (Maybe Int -> msg) -> Options msg
defaultOptions onInput =
    { onInput = onInput
    , maxLength = Nothing
    , maxValue = Nothing
    , minValue = Nothing
    , hasFocus = Nothing
    }


{-| Default options for input with `String` value
Params:

  - `onInput` (type: `String -> msg`) : The onInput Msg tagger

Value:

    { onInput = onInput
    , maxLength = Nothing
    , maxValue = Nothing
    , minValue = Nothing
    , hasFocus = Nothing
    }

-}
defaultStringOptions : (String -> msg) -> StringOptions msg
defaultStringOptions onInput =
    { onInput = onInput
    , maxLength = Nothing
    , maxValue = Nothing
    , minValue = Nothing
    , hasFocus = Nothing
    }


{-| View function

Example:

    type alias Model = { currentValue : Maybe Int }

    type Msg = InputUpdated (Maybe Int) | FocusUpdated Bool

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
input : Options msg -> List (Attribute msg) -> Maybe Int -> Html msg
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

        maxAttribute =
            options.maxValue
                |> Maybe.map String.fromInt
                |> Maybe.map Attributes.max
                |> Maybe.map toArray
                |> Maybe.withDefault []

        minAttribute =
            options.minValue
                |> Maybe.map String.fromInt
                |> Maybe.map Attributes.min
                |> Maybe.map toArray
                |> Maybe.withDefault []
    in
    Html.input
        (List.append attributes
            [ currentValue
                |> Maybe.map String.fromInt
                |> Maybe.withDefault ""
                |> value
            , onKeyDown options currentValue
            , Html.Events.onInput (String.toInt >> options.onInput)
            , onChange options
            , type_ "number"
            ]
            |> List.append onFocusAttribute
            |> List.append onBlurAttribute
            |> List.append maxAttribute
            |> List.append minAttribute
        )
        []


{-| View function for input with `String` value

Example:

    type alias Model = { currentValue : String }

    type Msg = InputUpdated String | FocusUpdated Bool

    Input.Number.inputString
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
inputString : StringOptions msg -> List (Attribute msg) -> String -> Html msg
inputString options attributes currentValue =
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

        maxAttribute =
            options.maxValue
                |> Maybe.map String.fromInt
                |> Maybe.map Attributes.max
                |> Maybe.map toArray
                |> Maybe.withDefault []

        minAttribute =
            options.minValue
                |> Maybe.map String.fromInt
                |> Maybe.map Attributes.min
                |> Maybe.map toArray
                |> Maybe.withDefault []
    in
    Html.input
        (List.append attributes
            [ currentValue
                |> value
            , onKeyDownString options currentValue
            , Html.Events.onInput options.onInput
            , onChangeString options
            , type_ "number"
            ]
            |> List.append onFocusAttribute
            |> List.append onBlurAttribute
            |> List.append maxAttribute
            |> List.append minAttribute
        )
        []


filterNonDigit : String -> String
filterNonDigit value =
    value |> String.toList |> List.filter Char.isDigit |> String.fromList


onKeyDownString : StringOptions msg -> String -> Attribute msg
onKeyDownString options currentValue =
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


onKeyDown : Options msg -> Maybe Int -> Attribute msg
onKeyDown options currentValue =
    let
        newValue keyCode =
            keyCode
                |> Char.fromCode
                |> String.fromChar
                |> (++) (Maybe.withDefault "" <| Maybe.map String.fromInt <| currentValue)

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
                    ( options.onInput (String.toInt <| newValue event.keyCode), False )

                else
                    ( options.onInput currentValue, True )

        decoder =
            eventDecoder
                |> Json.map filterKey
    in
    preventDefaultOn "keydown" decoder


isValid : String -> GenericOptions a -> Bool
isValid newValue options =
    let
        updatedNumber =
            newValue
                |> String.toInt
    in
    not (exceedMaxLength options.maxLength newValue)
        && not (exceedMaxValue options.maxValue updatedNumber)


onChange : Options msg -> Html.Attribute msg
onChange options =
    let
        checkWithMinValue number =
            if lessThanMinValue options.minValue number then
                options.minValue

            else
                number

        checkWithMaxValue number =
            if exceedMaxValue options.maxValue number then
                options.maxValue

            else
                number

        toInt string =
            string
                |> String.toInt
                |> checkWithMinValue
                |> checkWithMaxValue
    in
    Html.Events.on "change" (Json.map (toInt >> options.onInput) Html.Events.targetValue)


leadingZeroRegex : Regex.Regex
leadingZeroRegex =
    Regex.fromString "0*"
        |> Maybe.withDefault Regex.never


onChangeString : StringOptions msg -> Html.Attribute msg
onChangeString options =
    let
        checkWithMinValue number =
            if lessThanMinValue options.minValue number then
                options.minValue

            else
                number

        checkWithMaxValue number =
            if exceedMaxValue options.maxValue number then
                options.maxValue

            else
                number

        leadingZero string =
            Regex.findAtMost 1 leadingZeroRegex string
                |> List.head
                |> Maybe.map .match
                |> Maybe.withDefault ""

        toInt string =
            string
                |> String.toInt
                |> checkWithMinValue
                |> checkWithMaxValue
                |> Maybe.map String.fromInt
                |> Maybe.map (\a -> (++) a (leadingZero string))
    in
    Html.Events.on "change" (Json.map options.onInput Html.Events.targetValue)


lessThanMinValue : Maybe Int -> Maybe Int -> Bool
lessThanMinValue minValue maybeNumber =
    maybeNumber
        |> Maybe.map2 (\min number -> number < min) minValue
        |> Maybe.withDefault False


exceedMaxValue : Maybe Int -> Maybe Int -> Bool
exceedMaxValue maxValue maybeNumber =
    maybeNumber
        |> Maybe.map2 (\max number -> number > max) maxValue
        |> Maybe.withDefault False


exceedMaxLength : Maybe Int -> String -> Bool
exceedMaxLength maybeMaxLength value =
    maybeMaxLength
        |> Maybe.map (\maxLength -> maxLength >= String.length value)
        |> Maybe.map not
        |> Maybe.withDefault False
