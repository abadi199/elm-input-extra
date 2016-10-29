module Pattern.Pattern
    exposing
        ( parse
        , tokenize
        , Token(..)
        , format
        , extract
        )

{-| Pattern

# Functions
@docs parse, format, value
-}

import String


type Token
    = Input
    | Other Char


{-| Parse a String and return a pattern.

e.g.: parse "(###)" will return [ Other '(', Input, Input, Input, Other ')' ]
-}
parse : Char -> String -> List Token
parse inputChar pattern =
    String.toList pattern
        |> List.map (tokenize inputChar)


tokenize : Char -> Char -> Token
tokenize inputChar pattern =
    if pattern == inputChar then
        Input
    else
        Other pattern


{-| Format an input with a pattern
-}
format : List Token -> String -> String
format tokens input =
    append tokens (String.toList input) ""


append : List Token -> List Char -> String -> String
append tokens input formatted =
    let
        appendInput =
            List.head input
                |> Maybe.map (\char -> formatted ++ String.fromChar char)
                |> Maybe.map (append (Maybe.withDefault [] (List.tail tokens)) (Maybe.withDefault [] (List.tail input)))
                |> Maybe.withDefault formatted

        maybeToken =
            List.head tokens
    in
        case maybeToken of
            Nothing ->
                formatted

            Just token ->
                case token of
                    Input ->
                        appendInput

                    Other char ->
                        append (Maybe.withDefault [] <| List.tail tokens) input (formatted ++ String.fromChar char)


{-| Extract original value out of formatted string
-}
extract : List Token -> String -> Result String String
extract tokens formatted =
    scan tokens (String.toList formatted) ""


scan : List Token -> List Char -> String -> Result String String
scan tokens input value =
    let
        maybeToken =
            List.head tokens

        maybeInputChar =
            List.head input

        parseToken token inputChar =
            case token of
                Input ->
                    scan
                        (Maybe.withDefault [] (List.tail tokens))
                        (Maybe.withDefault [] (List.tail input))
                        (value ++ String.fromChar inputChar)

                Other other ->
                    if other == inputChar then
                        scan
                            (Maybe.withDefault [] (List.tail tokens))
                            (Maybe.withDefault [] (List.tail input))
                            (value)
                    else
                        Result.Err <| "Non-matching character for " ++ String.fromChar other
    in
        case maybeToken of
            Nothing ->
                Result.Ok value

            Just token ->
                maybeInputChar
                    |> Maybe.map (parseToken token)
                    |> Maybe.withDefault (Result.Ok value)
