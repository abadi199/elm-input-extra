module PatternTests exposing (..)

import Test exposing (..)
import Expect
import MaskedInput.Pattern as Pattern exposing (Token(..))
import String
import Diff


-- CONSTANTS


parensPattern : List Token
parensPattern =
    [ Other '(', Input, Input, Input, Other ')' ]


phonePattern : List Token
phonePattern =
    [ Other '(', Input, Input, Input, Other ')', Other ' ', Input, Input, Input, Other '-', Input, Input, Input, Input ]


datePattern : List Token
datePattern =
    [ Input, Input, Other '/', Input, Input, Other '/', Input, Input, Input, Input ]


all : Test
all =
    describe "Pattern Test Suite"
        [ parseTests
        , formatTests
        , extractTests
        , isValidTests
        , adjustTests
        , splitChangesTest
        , changesPairWithTokenTests
        , foldPairsTest
        ]



-- TEST SUITE


parseTests : Test
parseTests =
    describe "Pattern.parse"
        [ test "with pattern with '#' will return the right tokens" <|
            \() ->
                Pattern.parse '#' "(###)"
                    |> Expect.equal parensPattern
        , test "with pattern with '?' will return the right tokens" <|
            \() ->
                Pattern.parse '?' "??/??/????"
                    |> Expect.equal datePattern
        ]


formatTests : Test
formatTests =
    describe "Pattern.format"
        [ test "format an input with '(###)' pattern will return formatted text" <|
            \() ->
                Pattern.format parensPattern "123"
                    |> Expect.equal "(123)"
        , test "format an input longer that 3 characters with '(###)' pattern will return formatted text" <|
            \() ->
                Pattern.format parensPattern "123123123"
                    |> Expect.equal "(123)"
        , test "format an input with phone pattern will return formatted phone number" <|
            \() ->
                Pattern.format phonePattern "3145551234"
                    |> Expect.equal "(314) 555-1234"
        , test "format an insufficient input with '(###) ###-####' pattern will return partial result" <|
            \() ->
                Pattern.format phonePattern "31455"
                    |> Expect.equal "(314) 55"
        , test "format an empty string with phone pattern will return an empty string" <|
            \() ->
                Pattern.format phonePattern ""
                    |> Expect.equal ""
        , test "format an 6 characters input with phone number pattern should not end with '-'" <|
            \() ->
                Pattern.format phonePattern "314555"
                    |> String.endsWith "-"
                    |> Expect.true "Expected formatted should ends with '-'"
        ]


extractTests : Test
extractTests =
    describe "Pattern.extract"
        [ test "for parens pattern will extract the right value" <|
            \() ->
                Pattern.extract parensPattern "(123)"
                    |> Expect.equal "123"
        , test "for parens pattern will extract the right value" <|
            \() ->
                Pattern.extract parensPattern "(123"
                    |> Expect.equal "123"
        , test "for phone pattern will extract the right value" <|
            \() ->
                Pattern.extract phonePattern "(314) 555-1234"
                    |> Expect.equal "3145551234"
        , test "for phone pattern will extract the right value" <|
            \() ->
                Pattern.extract phonePattern "(314"
                    |> Expect.equal "314"
        , test "for incomplete phone pattern will extract the right value" <|
            \() ->
                Pattern.extract phonePattern "(314) 555"
                    |> Expect.equal "314555"
        , test "for longer phone pattern will extract the right value ignoring the rest of the input" <|
            \() ->
                Pattern.extract phonePattern "(314) 555-1234xxx"
                    |> Expect.equal "3145551234"
        , test "for phone pattern with wrong input will return an error" <|
            \() ->
                Pattern.extract phonePattern "12/12/2019"
                    |> Expect.equal "12/12/2019"
        ]


isValidTests : Test
isValidTests =
    describe "Pattern.isValid"
        [ test "for parens pattern with correct format should be True" <|
            \() ->
                Pattern.isValid "(123)" parensPattern
                    |> Expect.true "formatted value is valid"
        , test "for phone pattern with correct format should be True" <|
            \() ->
                Pattern.isValid "(314) 555-1234" phonePattern
                    |> Expect.true "formatted value is valid"
        , test "for phone pattern with partially correct format should be True" <|
            \() ->
                Pattern.isValid "(314) 555-1" phonePattern
                    |> Expect.true "formatted value is valid"
        , test "for parens pattern with incorrect format should be False" <|
            \() ->
                Pattern.isValid "(3145)" parensPattern
                    |> Expect.false "formatted value is invalid"
        , test "for phone pattern with incorrect format should be False" <|
            \() ->
                Pattern.isValid "(314)111122" phonePattern
                    |> Expect.false "formatted value is invalid"
        ]


adjustTests : Test
adjustTests =
    describe "Pattern.adjust"
        [ test "for parens pattern, from (12 to (123" <|
            \() ->
                Pattern.adjust parensPattern Pattern.OtherUpdate "(12" "(123"
                    |> Expect.equal "123"
        , test "for phone pattern, from (123) 45 to (1223) 45" <|
            \() ->
                Pattern.adjust phonePattern Pattern.Backspace "(123) 45" "(1223) 45"
                    |> Expect.equal "122345"
        , test "for parens pattern, from (123) to (13)" <|
            \() ->
                Pattern.adjust parensPattern Pattern.Backspace "(123)" "(13)"
                    |> Expect.equal "13"
        , test "for parens pattern, from (123) to (123" <|
            \() ->
                Pattern.adjust parensPattern Pattern.Backspace "(123)" "(123"
                    |> Expect.equal "12"
        , test "for parens pattern, from (123) to 123)" <|
            \() ->
                Pattern.adjust parensPattern Pattern.Backspace "(123)" "123)"
                    |> Expect.equal "123"
        , test "for parens pattern, from (123) to (123)5" <|
            \() ->
                Pattern.adjust parensPattern Pattern.Backspace "(123)" "(123)5"
                    |> Expect.equal "1235"
        ]


splitChangesTest : Test
splitChangesTest =
    describe "Pattern.splitChanges"
        [ test "Changes from (123) to (123A)" <|
            \() ->
                Pattern.splitChanges (Diff.diffChars "(123)" "(123A)")
                    |> Expect.equal
                        [ Diff.NoChange "("
                        , Diff.NoChange "1"
                        , Diff.NoChange "2"
                        , Diff.NoChange "3"
                        , Diff.Added "A"
                        , Diff.NoChange ")"
                        ]
        , test "Changes from (123) to (13)" <|
            \() ->
                Pattern.splitChanges (Diff.diffChars "(123)" "(13)")
                    |> Expect.equal
                        [ Diff.NoChange "("
                        , Diff.NoChange "1"
                        , Diff.Removed "2"
                        , Diff.NoChange "3"
                        , Diff.NoChange ")"
                        ]
        ]


changesPairWithTokenTests : Test
changesPairWithTokenTests =
    describe "Pattern.changesPairWithToken"
        [ test "Changes from (123) to (123A) with parensPattern" <|
            \() ->
                Pattern.changesPairWithToken parensPattern "(123)" "(123A)"
                    |> Expect.equal
                        [ ( Just <| Other '(', Diff.NoChange "(" )
                        , ( Just <| Input, Diff.NoChange "1" )
                        , ( Just <| Input, Diff.NoChange "2" )
                        , ( Just <| Input, Diff.NoChange "3" )
                        , ( Nothing, Diff.Added "A" )
                        , ( Just <| Other ')', Diff.NoChange ")" )
                        ]
        , test "Changes from (123) to (13) with parensPattern" <|
            \() ->
                Pattern.changesPairWithToken parensPattern "(123)" "(13)"
                    |> Expect.equal
                        [ ( Just <| Other '(', Diff.NoChange "(" )
                        , ( Just <| Input, Diff.NoChange "1" )
                        , ( Just <| Input, Diff.Removed "2" )
                        , ( Just <| Input, Diff.NoChange "3" )
                        , ( Just <| Other ')', Diff.NoChange ")" )
                        ]
        , test "Changes from (123) to (123 with parensPattern" <|
            \() ->
                Pattern.changesPairWithToken parensPattern "(123)" "(123"
                    |> Expect.equal
                        [ ( Just <| Other '(', Diff.NoChange "(" )
                        , ( Just <| Input, Diff.NoChange "1" )
                        , ( Just <| Input, Diff.NoChange "2" )
                        , ( Just <| Input, Diff.NoChange "3" )
                        , ( Just <| Other ')', Diff.Removed ")" )
                        ]
        , test "Changes from (123) to 123) with parensPattern" <|
            \() ->
                Pattern.changesPairWithToken parensPattern "(123)" "123)"
                    |> Expect.equal
                        [ ( Just <| Other '(', Diff.Removed "(" )
                        , ( Just Input, Diff.NoChange "1" )
                        , ( Just Input, Diff.NoChange "2" )
                        , ( Just Input, Diff.NoChange "3" )
                        , ( Just <| Other ')', Diff.NoChange ")" )
                        ]
        , test "Changes from (123) to 123) with parensPattern" <|
            \() ->
                Pattern.changesPairWithToken parensPattern "(123)" "123)"
                    |> Expect.equal
                        [ ( Just <| Other '(', Diff.Removed "(" )
                        , ( Just Input, Diff.NoChange "1" )
                        , ( Just Input, Diff.NoChange "2" )
                        , ( Just Input, Diff.NoChange "3" )
                        , ( Just <| Other ')', Diff.NoChange ")" )
                        ]
        , test "Changes from (12 to (123 with parensPattern" <|
            \() ->
                Pattern.changesPairWithToken parensPattern "(12" "(123"
                    |> Expect.equal
                        [ ( Just <| Other '(', Diff.NoChange "(" )
                        , ( Just Input, Diff.NoChange "1" )
                        , ( Just Input, Diff.NoChange "2" )
                        , ( Nothing, Diff.Added "3" )
                        ]
        ]


foldPairsTest : Test
foldPairsTest =
    describe "Pattern.foldPairs"
        [ test "Changes from (123) to (123A) with parensPattern" <|
            \() ->
                Pattern.foldPairs
                    Pattern.OtherUpdate
                    [ ( Just <| Other '(', Diff.NoChange "(" )
                    , ( Just <| Input, Diff.NoChange "1" )
                    , ( Just <| Input, Diff.NoChange "2" )
                    , ( Just <| Input, Diff.NoChange "3" )
                    , ( Nothing, Diff.Added "A" )
                    , ( Just <| Other ')', Diff.NoChange ")" )
                    ]
                    |> Expect.equal "123A"
        , test "Changes from (123) to (13) with parensPattern and backspace" <|
            \() ->
                Pattern.foldPairs
                    Pattern.Backspace
                    [ ( Just <| Other '(', Diff.NoChange "(" )
                    , ( Just <| Input, Diff.NoChange "1" )
                    , ( Just <| Input, Diff.Removed "2" )
                    , ( Just <| Input, Diff.NoChange "3" )
                    , ( Just <| Other ')', Diff.NoChange ")" )
                    ]
                    |> Expect.equal "13"
        , test "Changes from (123) to (123 with parensPattern and backspace" <|
            \() ->
                Pattern.foldPairs
                    Pattern.Backspace
                    [ ( Just <| Other '(', Diff.NoChange "(" )
                    , ( Just <| Input, Diff.NoChange "1" )
                    , ( Just <| Input, Diff.NoChange "2" )
                    , ( Just <| Input, Diff.NoChange "3" )
                    , ( Just <| Other ')', Diff.Removed ")" )
                    ]
                    |> Expect.equal "12"
        , test "Changes from (123) to 123) with parensPattern and backspace" <|
            \() ->
                Pattern.foldPairs
                    Pattern.Backspace
                    [ ( Just <| Other '(', Diff.Removed "(" )
                    , ( Just Input, Diff.NoChange "1" )
                    , ( Just Input, Diff.NoChange "2" )
                    , ( Just Input, Diff.NoChange "3" )
                    , ( Just <| Other ')', Diff.NoChange ")" )
                    ]
                    |> Expect.equal "123"
        , test "Changes from (123) to 123) with parensPattern and delete" <|
            \() ->
                Pattern.foldPairs
                    Pattern.Delete
                    [ ( Just <| Other '(', Diff.Removed "(" )
                    , ( Just Input, Diff.NoChange "1" )
                    , ( Just Input, Diff.NoChange "2" )
                    , ( Just Input, Diff.NoChange "3" )
                    , ( Just <| Other ')', Diff.NoChange ")" )
                    ]
                    |> Expect.equal "23"
        ]
