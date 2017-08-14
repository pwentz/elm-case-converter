module KebabTest exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Main exposing (..)
import Test exposing (..)


suite : Test
suite =
    describe "Kebab"
        [ describe "toKebab"
            [ test "it can convert title case" <|
                \_ ->
                    let
                        title =
                            "SomeThingVeryAwesome"

                        expected =
                            "some-thing-very-awesome"
                    in
                    Expect.equal expected (toKebab title)
            , test "it can convert title case with acronyms" <|
                \_ ->
                    let
                        title =
                            "SimpleHTTPRequest"

                        expected =
                            "simple-http-request"
                    in
                    Expect.equal expected (toKebab title)
            , test "it can convert camel case" <|
                \_ ->
                    let
                        camel =
                            "someThingVeryAwesome"

                        expected =
                            "some-thing-very-awesome"
                    in
                    Expect.equal expected (toKebab camel)
            , test "it can support acronyms" <|
                \_ ->
                    let
                        camel =
                            "simpleHttpRequest"

                        expected =
                            "simple-http-request"
                    in
                    Expect.equal expected (toKebab camel)
            , test "it can support either style of acronym" <|
                \_ ->
                    let
                        camel =
                            "simpleHTTPRequest"

                        expected =
                            "simple-http-request"
                    in
                    Expect.equal expected (toKebab camel)
            , test "it can support snake case" <|
                \_ ->
                    let
                        snake =
                            "some_thing_very_awesome"

                        expected =
                            "some-thing-very-awesome"
                    in
                    Expect.equal expected (toKebab snake)
            , test "it supports screaming snake case" <|
                \_ ->
                    let
                        snake =
                            "SOME_THING_VERY_AWESOME"

                        expected =
                            "some-thing-very-awesome"
                    in
                    Expect.equal expected (toKebab snake)
            , test "it returns same string if case is ambiguous" <|
                \_ ->
                    let
                        unknownCase =
                            "someThing_Inconsistent"
                    in
                    Expect.equal unknownCase (toKebab unknownCase)
            , test "it can convert multiple words with consistent casing" <|
                \_ ->
                    let
                        words =
                            "BlueCharlie RedFoxtrot TangoAlpha"

                        expected =
                            "blue-charlie red-foxtrot tango-alpha"
                    in
                    Expect.equal expected (toKebab words)
            , test "it cannot convert multiple words with inconsistent casing" <|
                \_ ->
                    let
                        words =
                            "blue_charlie redFoxtrot TangoAlpha"
                    in
                    Expect.equal words (toKebab words)
            ]
        ]
