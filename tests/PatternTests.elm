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
        , valueTests
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


valueTests : Test
valueTests =
    describe "Pattern.value"
        [ test "" <|
            \() ->
                Pattern.value parensPattern "(123)"
                    |> Expect.equal "123"
        ]
