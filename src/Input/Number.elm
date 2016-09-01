module Input.Number exposing (Model, Options, Msg, init, input, update, defaultOptions)

{-| Number input

# Model
@docs Model, init

# View
@docs input, Options, defaultOptions

# Update
@docs update

# Msg
@docs Msg
-}

import Html exposing (Attribute, Html)
import Html.Attributes exposing (style, type')
import Html.Events exposing (onWithOptions, keyCode, onInput, onFocus, onBlur)
import Html.Attributes as Attributes exposing (value)
import Char
import String
import Json.Decode as Json exposing ((:=))
import Input.Decoder exposing (eventDecoder)
import Input.KeyCode exposing (allowedKeyCodes)


{-| Options of the input component.

 * `id` is the id of the HTML element.
 * `maxLength` is the maximum number of character allowed in this input. Set to `Nothing` for no limit.
 * `maxValue` is the maximum number value allowed in this input. Set to `Nothing` for no limit.
 * `minValue` is the minimum number value allowed in this input. Set to `Nothing` for no limit.
-}
type alias Options =
    { id : String
    , maxLength : Maybe Int
    , maxValue : Maybe Int
    , minValue : Maybe Int
    }


{-| Default value for `Options`.
Params:
 * `id` (type: `String`) : The `id` of the number input element.

Value:

    { id = id
    , maxLength = Nothing
    , maxValue = Nothing
    , minValue = Nothing
    }

-}
defaultOptions : String -> Options
defaultOptions id =
    { id = id
    , maxLength = Nothing
    , maxValue = Nothing
    , minValue = Nothing
    }


{-| (TEA) Model record
Fields:
 * `value` : current value of the input element.
 * `hasFocus` : flag whether the input element has focus or not.
-}
type alias Model =
    { value : String
    , hasFocus : Bool
    }


{-| (TEA) Initial model constant

Value:

    { value = ""
    , hasFocus = False
    }

-}
init : Model
init =
    { value = ""
    , hasFocus = False
    }


{-| (TEA) View function

Example:

    Input.Number.input
        { id = "NumberInput"
        , maxLength = Just 4
        , maxValue = Nothing
        , minValue = Nothing
        }
        [ class "numberInput"
        ...
        ]
        model.numberModel

-}
input : Options -> List (Attribute Msg) -> Model -> Html Msg
input options attributes model =
    Html.input
        (List.append attributes
            [ Attributes.id options.id
            , value model.value
            , onKeyDown options model KeyDown
            , onInput (OnInput options)
            , onFocus (OnFocus True)
            , onBlur (OnFocus False)
            , type' "number"
            ]
        )
        []


{-| (TEA) Update function

Example:

    type Msg = UpdateNumber Input.Number.Msg

    update msg model =
        case msg of
            UpdateNumber numberMsg ->
                { model | numberModel = Input.Number.update numberMsg model.numberModel }

-}
update : Msg -> Model -> Model
update msg model =
    case msg of
        NoOp ->
            model

        KeyDown keyCode ->
            model

        OnInput options newValue ->
            if isValid newValue options then
                { model | value = newValue |> filterNonDigit }
            else
                model

        OnFocus hasFocus ->
            { model | hasFocus = hasFocus }


filterNonDigit : String -> String
filterNonDigit value =
    value |> String.toList |> List.filter Char.isDigit |> String.fromList


{-| (TEA) Opaque Msg types
-}
type Msg
    = NoOp
    | KeyDown Char.KeyCode
    | OnInput Options String
    | OnFocus Bool


onKeyDown : Options -> Model -> (Int -> msg) -> Attribute msg
onKeyDown options model tagger =
    let
        eventOptions =
            { stopPropagation = False
            , preventDefault = True
            }

        newValue keyCode =
            keyCode
                |> Char.fromCode
                |> String.fromChar
                |> (++) model.value

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
                    Err "modifier key is pressed"
                else if List.any ((==) event.keyCode) allowedKeyCodes then
                    Err "allowedKeys"
                else if
                    (isNumber event.keyCode || isNumPad event.keyCode)
                        && isValid (newValue event.keyCode) options
                then
                    Err "numeric"
                else
                    Ok event.keyCode
            )

        decoder =
            filterKey
                |> Json.customDecoder eventDecoder
                |> Json.map tagger
    in
        onWithOptions "keydown" eventOptions decoder


isValid : String -> Options -> Bool
isValid newValue options =
    let
        updatedNumber =
            newValue
                |> String.toInt
                |> Result.toMaybe

        exceedMaxValue =
            updatedNumber
                |> Maybe.map2 (\max number -> number > max) options.maxValue
                |> Maybe.withDefault False

        lessThanMinValue =
            updatedNumber
                |> Maybe.map2 (\min number -> number < min) options.minValue
                |> Maybe.withDefault False

        exceedMaxLength =
            options.maxLength
                |> Maybe.map (\maxLength -> maxLength >= (String.length newValue))
                |> Maybe.map not
                |> Maybe.withDefault False
    in
        not exceedMaxLength
            && not (exceedMaxValue)
            && not (lessThanMinValue)
