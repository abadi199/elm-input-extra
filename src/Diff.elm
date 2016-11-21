module Diff
    exposing
        ( diffChars
        , diffLines
        , Change(..)
        )

{-| NOTES: This is a copy of https://github.com/avh4/elm-diff/blob/master/src/Diff.elm with an upgrade to Elm 0.18. 
I need to copy it here since it's not upgraded to Elm 0.18 yet.
Replace this with package dependency once it's upgraded to Elm 0.18.
Functions to compare strings to produce a list of changes.  This is an
implementation of the [Hunt-McIlroy](http://en.wikipedia.org/wiki/Hunt%E2%80%93McIlroy_algorithm)
diff algorithm.

# Types and Constructors
@docs Change

# Diffing strings
@docs diffChars, diffLines

-}

import Debug
import Dict exposing (Dict)
import List
import Maybe
import String


{-| -}
type Change
    = NoChange String
    | Changed String String
    | Added String
    | Removed String


mergeAll : Change -> List Change -> List Change
mergeAll next list =
    case ( next, list ) of
        ( Added a, (Added b) :: rest ) ->
            Added (a ++ b) :: rest

        ( Added a, (Removed b) :: rest ) ->
            Changed b a :: rest

        ( Added a, (Changed b1 b2) :: rest ) ->
            Changed b1 (a ++ b2) :: rest

        ( Removed a, (Removed b) :: rest ) ->
            Removed (a ++ b) :: rest

        ( Removed a, (Added b) :: rest ) ->
            Changed a b :: rest

        ( Removed a, (Changed b1 b2) :: rest ) ->
            Changed (a ++ b1) b2 :: rest

        ( NoChange a, (NoChange b) :: rest ) ->
            NoChange (a ++ b) :: rest

        ( Changed a1 a2, (Changed b1 b2) :: rest ) ->
            Changed (a1 ++ b1) (a2 ++ b2) :: rest

        _ ->
            (next :: list)


type alias Cell =
    ( Int, List Change )


type From
    = UseBoth
    | UseA
    | UseB


score : Int -> Change -> Cell -> Cell
score add c ( s, cs ) =
    ( s + add, c :: cs )


scores : Maybe Cell -> Maybe Cell -> Maybe Cell -> ( From, Int, Change ) -> Maybe Cell
scores tl t l ( from, add, c ) =
    (case from of
        UseA ->
            t

        UseB ->
            l

        UseBoth ->
            tl
    )
        |> Maybe.map (score add c)


bestScore : Maybe Cell -> Maybe Cell -> Maybe Cell
bestScore ma mb =
    case ( ma, mb ) of
        ( m, Nothing ) ->
            m

        ( Nothing, m ) ->
            m

        ( Just ( sa, ca ), Just ( sb, cb ) ) ->
            if sb > sa then
                Just ( sb, cb )
            else
                Just ( sa, ca )


orCrash : Maybe a -> a
orCrash m =
    case m of
        Just a ->
            a

        _ ->
            Debug.crash "No options"


best : Maybe Cell -> Maybe Cell -> Maybe Cell -> String -> String -> Cell
best tl t l a b =
    choices a b
        |> List.map (scores tl t l)
        |> List.foldl bestScore Nothing
        |> orCrash



-- should only happen if the grid as initialized incorrectly


choices : String -> String -> List ( From, Int, Change )
choices a b =
    if a == b then
        [ ( UseA, 0, Removed a )
        , ( UseB, 0, Added b )
        , ( UseBoth, 1, NoChange a )
        ]
    else
        [ ( UseA, 0, Removed a )
        , ( UseB, 0, Added b )
        , ( UseBoth, 0, Changed a b )
        ]


type alias State =
    Dict ( Int, Int ) Cell


val : Int -> Int -> State -> Maybe Cell
val row col s =
    Dict.get ( row, col ) s


calcCell : ( Int, String ) -> ( Int, String ) -> State -> State
calcCell ( row, a ) ( col, b ) s =
    Dict.insert ( row, col )
        (best (val (row - 1) (col - 1) s)
            (val (row - 1) (col) s)
            (val (row) (col - 1) s)
            a
            b
        )
        s


calcRow : List String -> ( Int, String ) -> State -> State
calcRow bs ( row, a ) d =
    bs
        |> List.indexedMap (,)
        |> List.foldl (calcCell ( row, a )) d


initialGrid : List String -> List String -> State
initialGrid az bs =
    Dict.singleton ( -1, -1 ) ( 0, [] )
        |> calcRow bs ( -1, "" )
        |> (\d -> List.foldl (\a -> calcCell a ( -1, "" )) d (az |> List.indexedMap (,)))


calcGrid : List String -> List String -> State
calcGrid az bs =
    az
        |> List.indexedMap (,)
        |> List.foldl (calcRow bs) (initialGrid az bs)


diff : (String -> List String) -> String -> String -> List Change
diff tokenize a b =
    let
        az =
            tokenize a

        bs =
            tokenize b
    in
        if az == [] then
            List.map Added bs |> List.foldr mergeAll []
        else if bs == [] then
            List.map Removed az |> List.foldr mergeAll []
        else
            calcGrid az bs
                |> Dict.get ( -1 + List.length az, -1 + List.length bs )
                |> Maybe.map (\( score, changes ) -> changes)
                |> Maybe.withDefault []
                |> List.foldl mergeAll []



-- rediff1 : (String -> List String) -> Change -> List Change
-- rediff1 tokenize change = case change of
--   Changed a b -> diff tokenize a b
--   _ -> [change]
--
-- rediff : (String -> List String) -> List Change -> List Change
-- rediff tokenize input = input |> List.map (rediff1 tokenize) |> List.concat


{-| Diffs two strings, comparing character by charater.

    diffChars "abc" "aBcd"
      == [ NoChange "a", Changed "b" "B", NoChange "c", Added "d" ]
-}
diffChars : String -> String -> List Change
diffChars =
    diff (String.split "")


tokenizeLines : String -> List String
tokenizeLines s =
    let
        tokens =
            String.split "\n" s

        n =
            List.length tokens
    in
        if s == "" then
            []
        else
            tokens
                |> List.indexedMap
                    (\i s ->
                        if i < n - 1 then
                            s ++ "\n"
                        else
                            s
                    )


{-| Diffs two strings, comparing line by line.

    original = """Brian
    Sohie
    Oscar
    Stella
    Takis
    """

    changed = """BRIAN
    Stella
    Frosty
    Takis
    """

    diffLines original changed
      == [ Changed "Brian\nSohie\nOscar\n" "BRIAN\n"
          , NoChange "Stella\n"
          , Added "Frosty\n"
          , NoChange "Takis\n"
          ]
-}
diffLines : String -> String -> List Change
diffLines =
    diff tokenizeLines
