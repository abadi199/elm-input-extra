module Input.Text exposing (Model, Options, Msg, init, input, update, defaultOptions)

{-| Text input

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
import Html.Events exposing (onWithOptions, keyCode, onInput, onFocus, onBlur)
import Html.Attributes as Attributes exposing (value, id, type')
import Char exposing (fromCode, KeyCode)


{-| Options of the input component.

 * `id` is the id of the HTML element.
 * `maxLength` is the maximum number of character allowed in this input. Set to `Nothing` for no limit.
 * `maxValue` is the maximum number value allowed in this input. Set to `Nothing` for no limit.
 * `minValue` is the minimum number value allowed in this input. Set to `Nothing` for no limit.
-}
type alias Options =
    { id : String
    , maxLength : Maybe Int
    }


{-| Default value for `Options`.
Params:
 * `id` (type: `String`) : The `id` of the number input element.
-}
defaultOptions : String -> Options
defaultOptions id =
    { id = id
    , maxLength = Nothing
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
-}
init : Model
init =
    { value = ""
    , hasFocus = False
    }


{-| (TEA) View function
-}
input : Options -> List (Attribute Msg) -> Model -> Html Msg
input options attributes model =
    Html.input
        (List.append attributes
            [ id options.id
            , value model.value
            , onInput OnInput
            , onFocus (OnFocus True)
            , onBlur (OnFocus False)
            , type' "text"
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

        KeyDown _ ->
            model

        OnInput newValue ->
            { model | value = newValue }

        OnFocus hasFocus ->
            { model | hasFocus = hasFocus }


{-| (TEA) Opaque Msg types
-}
type Msg
    = NoOp
    | KeyDown KeyCode
    | OnInput String
    | OnFocus Bool
