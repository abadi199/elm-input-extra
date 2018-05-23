module Tests exposing (..)

import Test exposing (Test, describe)
import PatternTests


all : Test
all =
    describe "All Test Suite"
        [ PatternTests.all
        ]
