module CamelTest exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Main exposing (..)
import Test exposing (..)


suite : Test
suite =
    describe "Camel"
        [ describe "toCamel"
            [ test "it can convert title case to camel case" <|
                \_ ->
                    let
                        title =
                            "SomeThingVeryAwesome"

                        expected =
                            "someThingVeryAwesome"
                    in
                    Expect.equal expected (toCamel title)
            , test "it can convert kebab case to camel case" <|
                \_ ->
                    let
                        kebab =
                            "some-thing-very-awesome"

                        expected =
                            "someThingVeryAwesome"
                    in
                    Expect.equal expected (toCamel kebab)
            , test "it can convert snake case to camel case" <|
                \_ ->
                    let
                        snake =
                            "some_thing_very_awesome"

                        expected =
                            "someThingVeryAwesome"
                    in
                    Expect.equal expected (toCamel snake)
            , test "it can convert screaming snake case" <|
                \_ ->
                    let
                        snake =
                            "SOME_THING_VERY_AWESOME"

                        expected =
                            "someThingVeryAwesome"
                    in
                    Expect.equal expected (toCamel snake)
            , test "it does not convert ambiguous cases" <|
                \_ ->
                    let
                        unknownCase =
                            "some-ThingVery_strange"
                    in
                    Expect.equal unknownCase (toCamel unknownCase)
            , test "it can convert multiple words with consistent casing" <|
                \_ ->
                    let
                        words =
                            "BlueCharlie RedFoxtrot TangoAlpha"

                        expected =
                            "blueCharlie redFoxtrot tangoAlpha"
                    in
                    Expect.equal expected (toCamel words)
            , test "it canno convert multiple words with inconsistent casing" <|
                \_ ->
                    let
                        words =
                            "blue-charlie RedFoxtrot TANGO_ALPHA"
                    in
                    Expect.equal words (toCamel words)
            ]
        ]
