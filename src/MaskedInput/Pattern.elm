module MaskedInput.Pattern exposing
    ( parse, format
    , Adjustment(..), Token(..), adjust, changesPairWithToken, extract, foldPairs, isValid, splitChanges, tokenize
    )

{-| Pattern


# Functions

@docs parse, format, value

-}

import Diff
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
                            value

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
    changesPairWithToken tokens previous current
        |> foldPairs adjustment


splitChanges : List (Diff.Change String) -> List (Diff.Change String)
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
    in
    changes
        |> List.map split
        |> List.concat


isAdd : Diff.Change String -> Bool
isAdd change =
    case change of
        Diff.Added _ ->
            True

        _ ->
            False


changesPairWithToken : List Token -> String -> String -> List ( Maybe Token, Diff.Change String )
changesPairWithToken tokens previous current =
    let
        getToken index change =
            case change of
                Diff.Added _ ->
                    Nothing

                _ ->
                    let
                        tokenIndex =
                            if List.length tokens < String.length previous then
                                (List.length tokens - 1) - index

                            else
                                (String.length previous - 1) - index
                    in
                    getAt tokenIndex tokens

        splittedChanges =
            Diff.diff (String.split "" previous) (String.split "" current)
                |> splitChanges

        totalChanges =
            List.length splittedChanges

        toPair change results =
            let
                index =
                    results
                        |> List.filter (\( _, change_ ) -> not (isAdd change_))
                        |> List.length
            in
            ( getToken index change, change ) :: results

        checkForLength changes =
            if List.length changes > List.length tokens then
                List.filter (\( a, change ) -> not (isAdd change)) changes

            else
                changes
    in
    splittedChanges
        |> List.foldr toPair []
        |> checkForLength
        |> List.take (List.length tokens)


foldPairs : Adjustment -> List ( Maybe Token, Diff.Change String ) -> String
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

                ( Just (Other _), Diff.NoChange _ ) ->
                    str

                ( Just Input, Diff.Removed s ) ->
                    str

                ( Just Input, Diff.Added s ) ->
                    concat isLeft s str

                ( Just Input, Diff.NoChange s ) ->
                    concat isLeft s str

                ( Nothing, Diff.Removed s ) ->
                    str

                ( Nothing, Diff.Added s ) ->
                    concat isLeft s str

                ( Nothing, Diff.NoChange s ) ->
                    concat isLeft s str
    in
    case adjustment of
        Backspace ->
            left

        _ ->
            right


changedString : Diff.Change String -> String
changedString change =
    case change of
        Diff.NoChange str ->
            str

        Diff.Added str ->
            str

        Diff.Removed str ->
            str



-- Helpers


{-| Copied in from List.Extra to avoid the dependency
-}
getAt : Int -> List a -> Maybe a
getAt idx xs =
    if idx < 0 then
        Nothing

    else
        List.head <| List.drop idx xs
