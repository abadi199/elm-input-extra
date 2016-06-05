module Number exposing (Model, init, input, update)

{-| Number input

# Model
@docs Model, init

# View
@docs input

# Update
@docs update
-}

import Html exposing (Attribute, Html)
import Html.Attributes exposing (style)
import Html.Events exposing (onWithOptions, keyCode, onInput, onFocus, onBlur)
import Html.App as Html
import Html.Attributes as Attributes exposing (value)
import Char exposing (fromCode, KeyCode)
import String exposing (fromChar, slice)
import Json.Decode as Json exposing ((:=))


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
            { model | value = newValue }

        -- { model | value = model.value ++ String.right 1 newValue }
        OnFocus hasFocus ->
            { model | hasFocus = hasFocus }


type Msg
    = NoOp
    | KeyDown KeyCode
    | OnInput String
    | OnFocus Bool


main : Program Never
main =
    let
        formatter s =
            List.repeat (String.length s) '*'
                |> String.fromList

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
