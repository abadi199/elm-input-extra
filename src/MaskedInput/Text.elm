module MaskedInput.Text
    exposing
        ( Options
        , input
        , defaultOptions
        , State
        , initialState
        )

{-| Masked Text input

# State
@docs State, initialState

# View
@docs input, Options, defaultOptions
-}

import Html exposing (Attribute, Html)
import Html.Events exposing (onWithOptions, keyCode, onInput, onFocus, onBlur)
import Html.Attributes as Attributes exposing (value, id, type_)
import Char
import String
import Json.Decode as Json
import Input.Decoder exposing (eventDecoder)
import Input.KeyCode exposing (allowedKeyCodes)
import MaskedInput.Pattern as Pattern


{-| Options of the input component.

 * `pattern` is the pattern used to format the input value. e.g.: (###) ###-####
 * `inputCharacter`: is the special character used to represent user input. Default value: `#`
 * `toMsg`: is the Msg for updating internal `State` of the element.
 * `onInput` is the Msg tagger for the onInput event.
 * `hasFocus` is an optional Msg tagger for onFocus/onBlur event.
-}
type alias Options msg =
    { pattern : String
    , inputCharacter : Char
    , onInput : String -> msg
    , toMsg : State -> msg
    , hasFocus : Maybe (Bool -> msg)
    }


{-| Opaque type for storing local State
-}
type State
    = State (Maybe Char.KeyCode)


{-| Initial state
-}
initialState : State
initialState =
    State Nothing


{-| Default value for `Options`.
 * `onInput` (type: `String -> msg`) : The onInput Msg tagger
 * `toMsg` (type: `String -> msg`) : The Msg for updating internal `State` of the element.

Value:

    { pattern = ""
    , inputCharacter = '#'
    , onInput = onInput
    , toMsg = toMsg
    , hasFocus = Nothing
    }

-}
defaultOptions : (String -> msg) -> (State -> msg) -> Options msg
defaultOptions onInput toMsg =
    { pattern = ""
    , inputCharacter = '#'
    , onInput = onInput
    , toMsg = toMsg
    , hasFocus = Nothing
    }


{-| Text input element

Example:

    type Msg = InputUpdated String | StateUpdated MaskedInput.State | FocusUpdated Bool

    MaskedInput.Text.input
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
input : Options msg -> List (Attribute msg) -> State -> String -> Html msg
input options attributes state currentValue =
    let
        tokens =
            Pattern.parse options.inputCharacter options.pattern

        onFocusAttribute =
            options.hasFocus
                |> Maybe.map (\f -> f True)
                |> Maybe.map (onFocus)
                |> Maybe.map (flip (::) [])
                |> Maybe.withDefault []

        onBlurAttribute =
            options.hasFocus
                |> Maybe.map (\f -> f False)
                |> Maybe.map onBlur
                |> Maybe.map (flip (::) [])
                |> Maybe.withDefault []

        currentFormattedValue =
            Pattern.format tokens currentValue
    in
        Html.input
            ((List.append attributes
                [ value currentFormattedValue
                , onInput (processInput options tokens state currentFormattedValue)
                , onKeyDown currentFormattedValue tokens options.toMsg
                , onKeyPress currentFormattedValue tokens options.toMsg
                , type_ "text"
                ]
             )
                |> List.append onFocusAttribute
                |> List.append onBlurAttribute
            )
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
    in
        Pattern.adjust tokens adjustment oldValue value |> options.onInput


onKeyDown : String -> List Pattern.Token -> (State -> msg) -> Attribute msg
onKeyDown currentFormattedValue tokens toMsg =
    let
        eventOptions =
            { stopPropagation = False
            , preventDefault = False
            }

        filterKey =
            (\event ->
                Json.succeed event.keyCode
            )

        decoder =
            eventDecoder
                |> Json.andThen filterKey
                |> Json.map (\keyCode -> toMsg <| State <| Just keyCode)
    in
        onWithOptions "keydown" eventOptions decoder


onKeyPress : String -> List Pattern.Token -> (State -> msg) -> Attribute msg
onKeyPress currentFormattedValue tokens toMsg =
    let
        eventOptions =
            { stopPropagation = False
            , preventDefault = True
            }

        filterKey =
            (\event ->
                if event.ctrlKey || event.altKey then
                    Json.fail "modifier key is pressed"
                else if List.any ((==) event.keyCode) allowedKeyCodes then
                    Json.fail "not arrow"
                else if String.length currentFormattedValue < List.length tokens then
                    Json.fail "accepting more input"
                else
                    Json.succeed event.keyCode
            )

        decoder =
            eventDecoder
                |> Json.andThen filterKey
                |> Json.map (\keyCode -> toMsg <| State <| Just keyCode)
    in
        onWithOptions "keypress" eventOptions decoder
