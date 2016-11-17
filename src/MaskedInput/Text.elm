module MaskedInput.Text exposing (Options, input, defaultOptions)

{-| Masked Text input

# View
@docs input, Options, defaultOptions
-}

import Html exposing (Attribute, Html)
import Html.Events exposing (onWithOptions, keyCode, onInput, onFocus, onBlur)
import Html.Attributes as Attributes exposing (value, id, type')
import Char exposing (fromCode, KeyCode)
import String
import Json.Decode as Json
import Input.Decoder exposing (eventDecoder)
import Input.KeyCode exposing (allowedKeyCodes)
import MaskedInput.Pattern as Pattern


{-| Options of the input component.

 * `maxLength` is the maximum number of character allowed in this input. Set to `Nothing` for no limit.
 * `onInput` is the Msg tagger for the onInput event.
 * `hasFocus` is an optional Msg tagger for onFocus/onBlur event.
-}
type alias Options msg =
    { pattern : String
    , inputCharacter : Char
    , onInput : String -> msg
    , hasFocus : Maybe (Bool -> msg)
    }


{-| Default value for `Options`.
 * `onInput` (type: `String -> msg`) : The onInput Msg tagger

Value:

    { maxLength = Nothing
    , onInput = onInput
    , hasFocus = Nothing
    }

-}
defaultOptions : (String -> msg) -> Options msg
defaultOptions onInput =
    { pattern = ""
    , inputCharacter = '#'
    , onInput = onInput
    , hasFocus = Nothing
    }


{-| Text input element

Example:

    Input.Text.input
        { id = "TextInput"
        , maxLength = Just 4
        }
        [ class "textInput"
        ...
        ]
        model.textModel

-}
input : Options msg -> List (Attribute msg) -> String -> Html msg
input options attributes currentValue =
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
            Pattern.format tokens (Debug.log "currentValue" currentValue)
    in
        Html.input
            ((List.append attributes
                [ value currentFormattedValue
                , onInput (processInput options tokens currentFormattedValue)
                , onKeyDown currentFormattedValue tokens options.onInput
                , type' "text"
                ]
             )
                |> List.append onFocusAttribute
                |> List.append onBlurAttribute
            )
            []


processInput : Options msg -> List Pattern.Token -> String -> String -> msg
processInput options tokens oldValue value =
    let
        _ =
            Debug.log "processInput.oldValue" oldValue

        _ =
            Debug.log "processInput.value" value
    in
        Pattern.adjust tokens Pattern.Backspace oldValue value |> Debug.log "adjustedValue" |> options.onInput


onKeyDown : String -> List Pattern.Token -> (String -> msg) -> Attribute msg
onKeyDown currentFormattedValue tokens tagger =
    let
        _ =
            Debug.log "onKeyDown.currentFormattedValue" currentFormattedValue

        eventOptions =
            { stopPropagation = False
            , preventDefault = True
            }

        filterKey =
            (\event ->
                if event.ctrlKey || event.altKey then
                    Err "modifier key is pressed"
                else if List.any ((==) event.keyCode) allowedKeyCodes then
                    Err "not arrow"
                else if String.length currentFormattedValue < List.length tokens then
                    Err "accepting more input"
                else
                    Ok event.keyCode
            )

        decoder =
            filterKey
                |> Json.customDecoder eventDecoder
                |> Json.map (\_ -> tagger <| Pattern.extract tokens currentFormattedValue)
    in
        onWithOptions "keydown" eventOptions decoder


isValid : String -> List Pattern.Token -> Bool
isValid value tokens =
    Pattern.isValid (Debug.log "value" value) tokens |> Debug.log "isValid"
