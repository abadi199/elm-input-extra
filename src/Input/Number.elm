module Input.Number exposing (Options, input, defaultOptions)

{-| Number input

# View
@docs input, Options, defaultOptions
-}

import Html exposing (Attribute, Html)
import Html.Attributes exposing (style, type_)
import Html.Events exposing (onWithOptions, keyCode)
import Html.Attributes as Attributes exposing (value)
import Char
import String
import Json.Decode as Json
import Input.Decoder exposing (eventDecoder)
import Input.KeyCode exposing (allowedKeyCodes)


{-| Options of the input component.

 * `maxLength` is the maximum number of character allowed in this input. Set to `Nothing` for no limit.
 * `maxValue` is the maximum number value allowed in this input. Set to `Nothing` for no limit.
 * `minValue` is the minimum number value allowed in this input. Set to `Nothing` for no limit.
 * `onInput` is the Msg tagger for the onInput event.
 * `hasFocus` is an optional Msg tagger for onFocus/onBlur event.
-}
type alias Options msg =
    { maxLength : Maybe Int
    , maxValue : Maybe Int
    , minValue : Maybe Int
    , onInput : Maybe Int -> msg
    , hasFocus : Maybe (Bool -> msg)
    }


{-| Default value for `Options`.
Params:
 * `onInput` (type: `String -> msg`) : The onInput Msg tagger

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
input : Options msg -> List (Attribute msg) -> Maybe Int -> Html msg
input options attributes currentValue =
    let
        toArray =
            flip (::) []

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
                |> Maybe.map toString
                |> Maybe.map Html.Attributes.max
                |> Maybe.map toArray
                |> Maybe.withDefault []

        minAttribute =
            options.minValue
                |> Maybe.map toString
                |> Maybe.map Html.Attributes.min
                |> Maybe.map toArray
                |> Maybe.withDefault []
    in
        Html.input
            ((List.append attributes
                [ value <| Maybe.withDefault "" <| Maybe.map toString <| currentValue
                , onKeyDown options currentValue
                , Html.Events.onInput (String.toInt >> Result.toMaybe >> options.onInput)
                , onChange options
                , type_ "number"
                ]
             )
                |> List.append onFocusAttribute
                |> List.append onBlurAttribute
                |> List.append maxAttribute
                |> List.append minAttribute
            )
            []


filterNonDigit : String -> String
filterNonDigit value =
    value |> String.toList |> List.filter Char.isDigit |> String.fromList


onKeyDown : Options msg -> Maybe Int -> Attribute msg
onKeyDown options currentValue =
    let
        eventOptions =
            { stopPropagation = False
            , preventDefault = True
            }

        newValue keyCode =
            keyCode
                |> Char.fromCode
                |> String.fromChar
                |> (++) (Maybe.withDefault "" <| Maybe.map toString <| currentValue)

        updatedNumber keyCode =
            newValue keyCode
                |> String.toInt
                |> Result.toMaybe

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
            (\event ->
                if event.ctrlKey || event.altKey || event.metaKey then
                    Json.fail "modifier key is pressed"
                else if List.any ((==) event.keyCode) allowedKeyCodes then
                    Json.fail "allowedKeys"
                else if
                    (isNumber event.keyCode || isNumPad event.keyCode)
                        && isValid (newValue event.keyCode) options
                then
                    Json.fail "numeric"
                else
                    Json.succeed event.keyCode
            )

        decoder =
            eventDecoder
                |> Json.andThen filterKey
                |> Json.map (\_ -> options.onInput currentValue)
    in
        onWithOptions "keydown" eventOptions decoder


isValid : String -> Options msg -> Bool
isValid newValue options =
    let
        updatedNumber =
            newValue
                |> String.toInt
                |> Result.toMaybe
    in
        not (exceedMaxLength options newValue)
            && not (exceedMaxValue options updatedNumber)


onChange : Options msg -> Html.Attribute msg
onChange options =
    let
        checkWithMinValue number =
            if lessThanMinValue options number then
                options.minValue
            else
                number

        checkWithMaxValue number =
            if exceedMaxValue options number then
                options.maxValue
            else
                number

        toInt string =
            string
                |> String.toInt
                |> Result.toMaybe
                |> checkWithMinValue
                |> checkWithMaxValue
    in
        Html.Events.on "change" (Json.map (toInt >> options.onInput) Html.Events.targetValue)


lessThanMinValue : Options msg -> Maybe Int -> Bool
lessThanMinValue options number =
    number
        |> Maybe.map2 (\min number -> number < min) options.minValue
        |> Maybe.withDefault False


exceedMaxValue : Options msg -> Maybe Int -> Bool
exceedMaxValue options number =
    number
        |> Maybe.map2 (\max number -> number > max) options.maxValue
        |> Maybe.withDefault False


exceedMaxLength : Options msg -> String -> Bool
exceedMaxLength options value =
    options.maxLength
        |> Maybe.map (\maxLength -> maxLength >= (String.length value))
        |> Maybe.map not
        |> Maybe.withDefault False
