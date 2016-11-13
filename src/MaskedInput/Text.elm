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
            (Pattern.format tokens currentValue)
    in
        Html.input
            ((List.append attributes
                [ value currentFormattedValue
                , onKeyDown currentFormattedValue tokens options.onInput
                , onInput (processInput options tokens currentFormattedValue)
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
            Debug.log "oldValue" oldValue
    in
        Pattern.extract tokens (Debug.log "newValue" value) |> options.onInput


onKeyDown : String -> List Pattern.Token -> (String -> msg) -> Attribute msg
onKeyDown currentValue tokens tagger =
    let
        eventOptions =
            { stopPropagation = False
            , preventDefault = True
            }

        filterKey =
            (\event ->
                let
                    newValue =
                        (currentValue ++ (event.keyCode |> Char.fromCode |> String.fromChar))
                in
                    if event.ctrlKey || event.altKey then
                        Err "modifier key is pressed"
                    else if List.any ((==) event.keyCode) allowedKeyCodes then
                        Err "not arrow"
                    else if (isValid newValue tokens) then
                        Err "valid"
                    else
                        Ok event.keyCode
            )

        decoder =
            filterKey
                |> Json.customDecoder eventDecoder
                |> Json.map (\_ -> tagger currentValue)
    in
        onWithOptions "keydown" eventOptions decoder


isValid : String -> List Pattern.Token -> Bool
isValid value tokens =
    Pattern.isValid (Debug.log "value" value) tokens |> Debug.log "isValid"
