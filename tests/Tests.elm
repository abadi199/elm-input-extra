module Tests exposing (..)

import Test exposing (Test, describe)
import PatternTests
import DateUtilsTests


all : Test
all =
    describe "All Test Suite"
        [ PatternTests.all
        , DateUtilsTests.all
        ]
