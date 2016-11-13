module MaskedInput.Pattern
    exposing
        ( parse
        , tokenize
        , Token(..)
        , format
        , extract
        , isValid
        , adjust
        , Adjustment(..)
        , changes
        )

{-| Pattern

# Functions
@docs parse, format, value
-}

import String
import Diff
import List.Extra


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
    if String.isEmpty input then
        input
    else
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
extract : List Token -> String -> String
extract tokens formatted =
    scan tokens (String.toList formatted) ""


scan : List Token -> List Char -> String -> String
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
                        String.fromList input
    in
        case maybeToken of
            Nothing ->
                value

            Just token ->
                maybeInputChar
                    |> Maybe.map (parseToken token)
                    |> Maybe.withDefault value


isValid : String -> List Token -> Bool
isValid value tokens =
    let
        scanIsValid unscannedCharacters unscannedTokens =
            let
                currentToken =
                    List.head unscannedTokens

                currentCharacter =
                    List.head unscannedCharacters

                tailTokens =
                    List.tail unscannedTokens |> Maybe.withDefault []

                tailCharacters =
                    List.tail unscannedCharacters |> Maybe.withDefault []

                isTokenEmpty =
                    List.isEmpty unscannedCharacters

                isCharacterEmpty =
                    List.isEmpty unscannedCharacters
            in
                if isCharacterEmpty && isTokenEmpty then
                    True
                else if isCharacterEmpty && not isTokenEmpty then
                    True
                else if not isCharacterEmpty && isTokenEmpty then
                    False
                else
                    case currentToken of
                        Just Input ->
                            scanIsValid tailCharacters tailTokens

                        Just (Other other) ->
                            currentCharacter
                                |> Maybe.map ((==) other)
                                |> Maybe.map
                                    (\isMatch ->
                                        if isMatch then
                                            scanIsValid tailCharacters tailTokens
                                        else
                                            False
                                    )
                                |> Maybe.withDefault False

                        Nothing ->
                            False
    in
        scanIsValid (String.toList value) tokens


type Adjustment
    = Backspace
    | Delete
    | OtherUpdate


adjust : List Token -> Adjustment -> String -> String -> String
adjust tokens adjustment previous current =
    let
        previousValue =
            extract tokens (Debug.log "previous" previous)
                |> Debug.log "previousValue"

        changedPositions =
            changes previous current
                |> Debug.log "changedPosition"

        changesMappedToTokens =
            changedPositions
                |> List.filterMap (\index -> List.Extra.getAt index tokens)
                |> Debug.log "changesMappedToTokens"
    in
        ""


changes : String -> String -> List Int
changes previous current =
    let
        changedString change =
            case change of
                Diff.NoChange str ->
                    str

                Diff.Added str ->
                    str

                Diff.Removed str ->
                    str

                Diff.Changed str _ ->
                    str

        foldDiff change ( index, result ) =
            let
                updatedIndex =
                    String.length (changedString change) + index
            in
                ( updatedIndex
                , case change of
                    Diff.NoChange _ ->
                        result

                    _ ->
                        List.append result [ updatedIndex ]
                )
    in
        Diff.diffChars previous current
            |> Debug.log "diffChars"
            |> List.foldl foldDiff ( -1, [] )
            |> snd
