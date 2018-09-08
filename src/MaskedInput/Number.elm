module MaskedInput.Number exposing
    ( State, initialState
    , input, Options, defaultOptions
    )

{-| Masked Number input, similar to Masked Text input, but only accepting numeric input


# State

@docs State, initialState


# View

@docs input, Options, defaultOptions

-}

import Char
import Html exposing (Attribute, Html)
import Html.Attributes as Attributes exposing (id, type_, value)
import Html.Events exposing (keyCode, on, onBlur, onFocus, onInput, preventDefaultOn)
import Input.Decoder exposing (eventDecoder)
import Input.KeyCode exposing (allowedKeyCodes)
import Json.Decode as Json
import MaskedInput.Pattern as Pattern
import String


{-| Options of the input component.

  - `pattern` is the pattern used to format the input value. e.g.: (###) ###-####
  - `inputCharacter`: is the special character used to represent user input. Default value: `#`
  - `toMsg`: is the Msg for updating internal `State` of the element.
  - `onInput` is the Msg tagger for the onInput event.
  - `hasFocus` is an optional Msg tagger for onFocus/onBlur event.

-}
type alias Options msg =
    { pattern : String
    , inputCharacter : Char
    , onInput : Maybe Int -> msg
    , toMsg : State -> msg
    , hasFocus : Maybe (Bool -> msg)
    }


{-| Opaque type for storing local State
-}
type State
    = State (Maybe Int)


{-| Initial state
-}
initialState : State
initialState =
    State Nothing


{-| Default value for `Options`.

  - `onInput` (type: `Maybe Int -> msg`) : The onInput Msg tagger
  - `toMsg` (type: `String -> msg`) : The Msg for updating internal `State` of the element.

Value:

    { pattern = ""
    , inputCharacter = '#'
    , onInput = onInput
    , toMsg = toMsg
    , hasFocus = Nothing
    }

-}
defaultOptions : (Maybe Int -> msg) -> (State -> msg) -> Options msg
defaultOptions onInput toMsg =
    { pattern = ""
    , inputCharacter = '#'
    , onInput = onInput
    , toMsg = toMsg
    , hasFocus = Nothing
    }


{-| Masked Number input element

Example:

    type Msg = InputUpdated (Maybe Int) | StateUpdated MaskedInput.State | FocusUpdated Bool

    MaskedInput.Number.input
        { pattern = "(###) ###-####"
        , inputCharacter = '#'
        , onInput = InputUpdated
        , toMsg = StateUpdated
        , hasFocus = Just FocusUpdated
        }
        [ class "masked-input"
        ...
        ]
        model.currentState
        model.currentValue

-}
input : Options msg -> List (Attribute msg) -> State -> Maybe Int -> Html msg
input options attributes state currentValue =
    let
        tokens =
            Pattern.parse options.inputCharacter options.pattern

        onFocusAttribute =
            options.hasFocus
                |> Maybe.map (\f -> f True)
                |> Maybe.map onFocus
                |> Maybe.map (\a -> (::) a [])
                |> Maybe.withDefault []

        onBlurAttribute =
            options.hasFocus
                |> Maybe.map (\f -> f False)
                |> Maybe.map onBlur
                |> Maybe.map (\a -> (::) a [])
                |> Maybe.withDefault []

        currentFormattedValue =
            currentValue
                |> Maybe.map String.fromInt
                |> Maybe.withDefault ""
                |> Pattern.format tokens

        inputAttributes =
            List.append attributes
                [ value currentFormattedValue
                , onInput (processInput options tokens state currentFormattedValue)
                , onKeyDown currentFormattedValue tokens options.toMsg
                , onKeyPress currentFormattedValue tokens options.toMsg
                , type_ "text"
                ]
                |> List.append onFocusAttribute
                |> List.append onBlurAttribute
    in
    Html.input
        inputAttributes
        []


processInput : Options msg -> List Pattern.Token -> State -> String -> String -> msg
processInput options tokens state oldValue value =
    let
        adjustment =
            case state of
                State (Just 8) ->
                    Pattern.Backspace

                State (Just 46) ->
                    Pattern.Delete

                _ ->
                    Pattern.OtherUpdate

        oldNumber =
            oldValue
                |> Pattern.extract tokens
                |> String.toInt

        newValue =
            Pattern.adjust tokens adjustment oldValue value
                |> Pattern.format tokens
                |> Pattern.extract tokens

        newNumber =
            newValue
                |> String.toInt
    in
    case ( newValue, newNumber ) of
        ( "", _ ) ->
            options.onInput Nothing

        ( _, Just _ ) ->
            options.onInput newNumber

        ( _, Nothing ) ->
            options.onInput oldNumber


onKeyDown : String -> List Pattern.Token -> (State -> msg) -> Attribute msg
onKeyDown currentFormattedValue tokens toMsg =
    let
        filterKey =
            \event ->
                Json.succeed event.keyCode

        decoder =
            eventDecoder
                |> Json.andThen filterKey
                |> Json.map (\keyCode -> toMsg <| State <| Just keyCode)
    in
    on "keydown" decoder


onKeyPress : String -> List Pattern.Token -> (State -> msg) -> Attribute msg
onKeyPress currentFormattedValue tokens toMsg =
    let
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
                if event.ctrlKey || event.altKey then
                    ( event.keyCode, False )
                    -- Json.fail "modifier key is pressed"

                else if List.any ((==) event.keyCode) allowedKeyCodes then
                    ( event.keyCode, False )
                    -- Json.fail "not arrow"

                else if isNumber event.keyCode || isNumPad event.keyCode then
                    ( event.keyCode, False )
                    -- Json.fail "numeric"

                else if String.length currentFormattedValue < List.length tokens then
                    ( event.keyCode, False )
                    -- Json.fail "accepting more input"

                else
                    ( event.keyCode, True )

        -- Json.succeed event.keyCode
        decoder =
            eventDecoder
                |> Json.map filterKey
                |> Json.map (\( keyCode, preventDefault ) -> ( toMsg <| State <| Just keyCode, preventDefault ))
    in
    preventDefaultOn "keypress" decoder
