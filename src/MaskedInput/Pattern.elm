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
        , process
        , splitChanges
        , changesPairWithToken
        , foldPairs
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
        diff =
            Diff.diffChars previous current

        processed =
            process tokens diff []
                |> Debug.log "============="

        fold change str =
            case change of
                Diff.Removed _ ->
                    str

                _ ->
                    str ++ changedString change

        changesIndex =
            changes previous current
    in
        processed
            |> List.foldl fold ""
            |> format tokens


splitChanges : List Diff.Change -> List Diff.Change
splitChanges changes =
    let
        splitString change str =
            String.toList str
                |> List.map String.fromChar
                |> List.map change

        split change =
            case change of
                Diff.NoChange str ->
                    splitString Diff.NoChange str

                Diff.Added str ->
                    splitString Diff.Added str

                Diff.Removed str ->
                    splitString Diff.Removed str

                Diff.Changed _ str ->
                    splitString (Diff.Changed "") str
    in
        changes
            |> List.map split
            |> List.concat


isAdd : Diff.Change -> Bool
isAdd change =
    case change of
        Diff.Added _ ->
            True

        _ ->
            False


changesPairWithToken : List Token -> List Diff.Change -> List ( Maybe Token, Diff.Change )
changesPairWithToken tokens changes =
    let
        getToken index change =
            case change of
                Diff.Added _ ->
                    Nothing

                _ ->
                    let
                        tokenIndex =
                            ((List.length tokens) - 1) - index
                    in
                        List.Extra.getAt tokenIndex tokens

        splittedChanges =
            changes
                |> splitChanges

        totalChanges =
            List.length splittedChanges

        toPair change results =
            let
                index =
                    results
                        |> List.filter (\( _, change ) -> not (isAdd change))
                        |> List.length
                        |> (\length -> length)
            in
                ( getToken index change, change ) :: results
    in
        splittedChanges |> List.foldr toPair []


foldPairs : Adjustment -> List ( Maybe Token, Diff.Change ) -> String
foldPairs adjustment pairs =
    let
        left =
            List.foldl (fold True) "" pairs

        right =
            List.foldr (fold False) "" pairs

        concat isLeft s str =
            if isLeft then
                str ++ s
            else
                s ++ str

        fold isLeft pair str =
            case pair of
                ( Just (Other _), Diff.Removed s ) ->
                    if isLeft then
                        String.dropRight 1 str
                    else
                        String.dropLeft 1 str

                ( Just (Other _), Diff.Added _ ) ->
                    str

                ( Just (Other _), Diff.Changed _ _ ) ->
                    str

                ( Just (Other _), Diff.NoChange _ ) ->
                    str

                ( Just Input, Diff.Removed s ) ->
                    str

                ( Just Input, Diff.Added s ) ->
                    concat isLeft s str

                ( Just Input, Diff.Changed _ s ) ->
                    concat isLeft s str

                ( Just Input, Diff.NoChange s ) ->
                    concat isLeft s str

                ( Nothing, Diff.Removed s ) ->
                    str

                ( Nothing, Diff.Added s ) ->
                    concat isLeft s str

                ( Nothing, Diff.Changed _ s ) ->
                    concat isLeft s str

                ( Nothing, Diff.NoChange s ) ->
                    concat isLeft s str
    in
        case adjustment of
            Backspace ->
                left

            _ ->
                right


process : List Token -> List Diff.Change -> List Diff.Change -> List Diff.Change
process tokens unprocessed processed =
    let
        _ =
            Debug.log "unprocessed" unprocessed

        _ =
            Debug.log "processed" processed

        changeFilter change =
            case change of
                Diff.NoChange _ ->
                    True

                Diff.Removed _ ->
                    True

                _ ->
                    False
    in
        case unprocessed of
            [] ->
                []

            head :: tail ->
                let
                    extracted =
                        List.append processed [ head ]
                            |> List.filter changeFilter
                            |> List.map changedString
                            |> List.foldl (flip (++)) ""
                            |> extract tokens
                            |> Debug.log "extracted"

                    extractedProcessed =
                        processed
                            |> List.filter changeFilter
                            |> List.map changedString
                            |> List.foldl (flip (++)) ""
                            |> extract tokens
                            |> Debug.log "extractedProcessed"

                    extractedHead =
                        String.dropLeft (String.length extractedProcessed) extracted
                            |> Debug.log "extractedHead"
                in
                    case head of
                        Diff.NoChange _ ->
                            [ Diff.NoChange extractedHead ] ++ process tokens tail (processed ++ [ head ])

                        _ ->
                            [ head ] ++ process tokens tail (processed ++ [ head ])


changedString : Diff.Change -> String
changedString change =
    case change of
        Diff.NoChange str ->
            str

        Diff.Added str ->
            str

        Diff.Removed str ->
            str

        Diff.Changed _ str ->
            str


changes : String -> String -> List Int
changes previous current =
    let
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
            |> List.foldl foldDiff ( -1, [] )
            |> snd
