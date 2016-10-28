module Pattern.Pattern exposing (parse, tokenize, Token(..), format, value)

{-| Pattern

# Functions
@docs parse, format, value
-}

import String


{-| Parse a String and return a pattern.

e.g.: parse "(###)" will return [ Other '(', Input, Input, Input, Other ')' ]
-}
parse : Char -> String -> List Token
parse inputChar pattern =
    String.toList pattern
        |> List.map (tokenize inputChar)


{-| Format an input with a pattern
-}
format : List Token -> String -> String
format tokens input =
    append tokens (String.toList input) ""


{-| Extract original value out of formatted string
-}
value : List Token -> String -> String
value tokens formatted =
    formatted


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


tokenize : Char -> Char -> Token
tokenize inputChar pattern =
    if pattern == inputChar then
        Input
    else
        Other pattern


type Token
    = Input
    | Other Char
