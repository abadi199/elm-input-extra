module Input.Float exposing
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
import Html.Attributes as Attributes exposing (step, style, type_, value)
import Html.Events exposing (keyCode, preventDefaultOn)
import Input.Decoder exposing (eventDecoder)
import Input.KeyCode exposing (allowedKeyCodes)
import Json.Decode as Json
import Regex
import String


type alias GenericOptions options =
    { options
        | maxValue : Maybe Float
        , minValue : Maybe Float
    }


{-| Options of the input component.

  - `maxLength` is the maximum number of character allowed in this input. Set to `Nothing` for no limit.
  - `maxValue` is the maximum number value allowed in this input. Set to `Nothing` for no limit.
  - `minValue` is the minimum number value allowed in this input. Set to `Nothing` for no limit.
  - `onInput` is the Msg tagger for the onInput event.
  - `hasFocus` is an optional Msg tagger for onFocus/onBlur event.

-}
type alias Options msg =
    { maxValue : Maybe Float
    , minValue : Maybe Float
    , onInput : Maybe Float -> msg
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
    { maxValue : Maybe Float
    , minValue : Maybe Float
    , onInput : String -> msg
    , hasFocus : Maybe (Bool -> msg)
    }


{-| Default value for `Options`.
Params:

  - `onInput` (type: `Maybe Float -> msg`) : The onInput Msg tagger

Value:

    { onInput = onInput
    , maxValue = Nothing
    , minValue = Nothing
    , hasFocus = Nothing
    }

-}
defaultOptions : (Maybe Float -> msg) -> Options msg
defaultOptions onInput =
    { onInput = onInput
    , maxValue = Nothing
    , minValue = Nothing
    , hasFocus = Nothing
    }


{-| Default options for input with `String` value
Params:

  - `onInput` (type: `String -> msg`) : The onInput Msg tagger

Value:

    { onInput = onInput
    , maxValue = Nothing
    , minValue = Nothing
    , hasFocus = Nothing
    }

-}
defaultStringOptions : (String -> msg) -> StringOptions msg
defaultStringOptions onInput =
    { onInput = onInput
    , maxValue = Nothing
    , minValue = Nothing
    , hasFocus = Nothing
    }


{-| View function

Example:

    type alias Model = { currentValue : Maybe Float }

    type Msg = InputUpdated (Maybe Float) | FocusUpdated Bool

    Input.Number.input
        { onInput = InputUpdated
        , maxValue = 1000
        , minValue = 10
        , hasFocus = Just FocusUpdated
        }
        [ class "numberInput"
        ...
        ]
        model.currentValue

-}
input : Options msg -> List (Attribute msg) -> Maybe Float -> Html msg
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
                |> Maybe.map String.fromFloat
                |> Maybe.map Attributes.max
                |> Maybe.map toArray
                |> Maybe.withDefault []

        minAttribute =
            options.minValue
                |> Maybe.map String.fromFloat
                |> Maybe.map Attributes.min
                |> Maybe.map toArray
                |> Maybe.withDefault []
    in
    Html.input
        (List.append attributes
            [ currentValue
                |> Maybe.map String.fromFloat
                |> Maybe.withDefault ""
                |> value
            , onKeyDown options currentValue
            , Html.Events.onInput (String.toFloat >> options.onInput)
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
                |> Maybe.map String.fromFloat
                |> Maybe.map Attributes.max
                |> Maybe.map toArray
                |> Maybe.withDefault []

        minAttribute =
            options.minValue
                |> Maybe.map String.fromFloat
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
            , step "0.01"
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
            (keyCode
                >= 48
                && keyCode
                <= 57
            )
                || keyCode
                == 190

        filterKey =
            \event ->
                if event.ctrlKey || event.altKey || event.metaKey then
                    ( options.onInput currentValue, False )

                else if event.shiftKey then
                    ( options.onInput currentValue, True )

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


onKeyDown : Options msg -> Maybe Float -> Attribute msg
onKeyDown options currentValue =
    let
        newValue keyCode =
            keyCode
                |> Char.fromCode
                |> String.fromChar
                |> (++) (Maybe.withDefault "" <| Maybe.map String.fromFloat <| currentValue)

        isNumPad keyCode =
            keyCode
                >= 96
                && keyCode
                <= 105

        isNumber keyCode =
            (keyCode
                >= 48
                && keyCode
                <= 57
            )
                || keyCode
                == 190

        filterKey =
            \event ->
                if event.ctrlKey || event.altKey || event.metaKey then
                    ( options.onInput currentValue, False )

                else if event.shiftKey then
                    ( options.onInput currentValue, True )

                else if List.any ((==) event.keyCode) allowedKeyCodes then
                    ( options.onInput currentValue, False )

                else if
                    (isNumber event.keyCode || isNumPad event.keyCode)
                        && isValid (newValue event.keyCode) options
                then
                    ( options.onInput (newValue event.keyCode |> String.toFloat), False )

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
                |> String.toFloat
    in
    not (exceedMaxValue options.maxValue updatedNumber)


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

        toFloat string =
            string
                |> String.toFloat
                |> checkWithMinValue
                |> checkWithMaxValue
    in
    Html.Events.on "change" (Json.map (toFloat >> options.onInput) Html.Events.targetValue)


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

        toFloat string =
            string
                |> String.toFloat
                |> checkWithMinValue
                |> checkWithMaxValue
                |> Maybe.map String.fromFloat
                |> Maybe.map (\a -> (++) a (leadingZero string))
    in
    Html.Events.on "change" (Json.map options.onInput Html.Events.targetValue)


lessThanMinValue : Maybe Float -> Maybe Float -> Bool
lessThanMinValue minValue maybeNumber =
    maybeNumber
        |> Maybe.map2 (\min number -> number < min) minValue
        |> Maybe.withDefault False


exceedMaxValue : Maybe Float -> Maybe Float -> Bool
exceedMaxValue maxValue maybeNumber =
    maybeNumber
        |> Maybe.map2 (\max number -> number > max) maxValue
        |> Maybe.withDefault False
