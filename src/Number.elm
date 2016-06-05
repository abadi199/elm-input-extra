module Number exposing (Model, Options, init, input, update)

{-| Number input

# Model
@docs Model, init

# View
@docs input, Options

# Update
@docs update
-}

import Html exposing (Attribute, Html)
import Html.Attributes exposing (style)
import Html.Events exposing (onWithOptions, keyCode, onInput, onFocus, onBlur)
import Html.App as Html
import Html.Attributes as Attributes exposing (value)
import Char
import String
import Json.Decode as Json exposing ((:=))


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


{-| (TEA) Model record
-}
type alias Model =
    { value : String
    , hasFocus : Bool
    }


{-| (TEA) Initial model constant
-}
init : Model
init =
    { value = ""
    , hasFocus = False
    }


type alias Event =
    { keyCode : Int
    , ctrlKey : Bool
    , altKey : Bool
    }


allowedKeyCodes : List Int
allowedKeyCodes =
    [ 37, 39, 8, 17, 18, 46, 9, 13 ]


onKeyDown : Options -> Model -> (Int -> msg) -> Attribute msg
onKeyDown options model tagger =
    let
        eventDecoder =
            Json.object3 Event
                ("keyCode" := Json.int)
                ("ctrlKey" := Json.bool)
                ("altKey" := Json.bool)

        eventOptions =
            { stopPropagation = False
            , preventDefault = True
            }

        updatedNumber keyCode =
            keyCode
                |> Char.fromCode
                |> String.fromChar
                |> (++) model.value
                |> String.toInt
                |> Result.toMaybe

        exceedMaxValue keyCode =
            keyCode
                |> updatedNumber
                |> Maybe.map2 (\max number -> number > max) options.maxValue
                |> Maybe.withDefault False

        lessThanMinValue keyCode =
            keyCode
                |> updatedNumber
                |> Maybe.map2 (\min number -> number < min) options.minValue
                |> Maybe.withDefault False

        exceedMaxLength =
            options.maxLength
                |> Maybe.map ((>=) (String.length model.value))
                |> Maybe.withDefault True

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
                if event.ctrlKey || event.altKey then
                    Err "modifier key is pressed"
                else if List.any ((==) event.keyCode) allowedKeyCodes then
                    Err "not arrow"
                else if
                    (isNumber event.keyCode || isNumPad event.keyCode)
                        && not exceedMaxLength
                        && not (exceedMaxValue event.keyCode)
                        && not (lessThanMinValue event.keyCode)
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


{-| (TEA) View function
-}
input : Options -> List (Attribute Msg) -> Model -> Html Msg
input options attributes model =
    Html.input
        (List.append attributes
            [ Attributes.id options.id
            , value model.value
            , onKeyDown options model KeyDown
            , onInput OnInput
            , onFocus (OnFocus True)
            , onBlur (OnFocus False)
            ]
        )
        []


{-| (TEA) Update function
-}
update : Msg -> Model -> Model
update msg model =
    case msg of
        NoOp ->
            model

        KeyDown keyCode ->
            model

        OnInput newValue ->
            { model | value = newValue |> filterNonDigit }

        OnFocus hasFocus ->
            { model | hasFocus = hasFocus }


filterNonDigit : String -> String
filterNonDigit value =
    value |> String.toList |> List.filter Char.isDigit |> String.fromList


type Msg
    = NoOp
    | KeyDown Char.KeyCode
    | OnInput String
    | OnFocus Bool


main : Program Never
main =
    let
        view model =
            Html.form []
                [ Html.p []
                    [ input
                        { id = "NumberInput"
                        , maxLength = Just 16
                        , maxValue = Nothing
                        , minValue = Nothing
                        }
                        [ style
                            [ ( "border", "1px solid #ccc" )
                            , ( "padding", ".5rem" )
                            , ( "box-shadow", "inset 0 1px 1px rgba(0,0,0,.075);" )
                            ]
                        ]
                        model
                    , Html.text model.value
                    ]
                ]
    in
        Html.beginnerProgram
            { model = init
            , update = update
            , view = view
            }
