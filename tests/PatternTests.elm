module PatternTests exposing (..)

import Test exposing (..)
import Expect
import Pattern.Pattern as Pattern exposing (Token(..))


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
        ]


extractTests : Test
extractTests =
    describe "Pattern.extract"
        [ test "for parens pattern will extract the right value" <|
            \() ->
                Pattern.extract parensPattern "(123)"
                    |> Expect.equal (Result.Ok "123")
        , test "for phone pattern will extract the right value" <|
            \() ->
                Pattern.extract phonePattern "(314) 555-1234"
                    |> Expect.equal (Result.Ok "3145551234")
        , test "for incomplete phone pattern will extract the right value" <|
            \() ->
                Pattern.extract phonePattern "(314) 555"
                    |> Expect.equal (Result.Ok "314555")
        , test "for longer phone pattern will extract the right value ignoring the rest of the input" <|
            \() ->
                Pattern.extract phonePattern "(314) 555-1234xxx"
                    |> Expect.equal (Result.Ok "3145551234")
        , test "for phone pattern with wrong input will return an error" <|
            \() ->
                Pattern.extract phonePattern "12/12/2019"
                    |> Result.toMaybe
                    |> Expect.equal Nothing
        ]
