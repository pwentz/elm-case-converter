module SnakeTest exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Main exposing (..)
import Test exposing (..)


suite : Test
suite =
    describe "Snake"
        [ describe "toSnake"
            [ test "it can convert title case to snake case" <|
                \_ ->
                    let
                        title =
                            "SomeThingVeryCool"

                        expected =
                            "some_thing_very_cool"
                    in
                    Expect.equal expected (toSnake title)
            , test "it can convert title case with acronyms" <|
                \_ ->
                    let
                        title =
                            "SimpleHTTPRequest"

                        expected =
                            "simple_http_request"
                    in
                    Expect.equal expected (toSnake title)
            , test "it can convert camel case to snake case" <|
                \_ ->
                    let
                        title =
                            "someThingVeryCool"

                        expected =
                            "some_thing_very_cool"
                    in
                    Expect.equal expected (toSnake title)
            , test "it can convert kebab case to snake case" <|
                \_ ->
                    let
                        title =
                            "some-thing-very-cool"

                        expected =
                            "some_thing_very_cool"
                    in
                    Expect.equal expected (toSnake title)
            , test "it returns given string on ambiguous casing" <|
                \_ ->
                    let
                        word =
                            "SomeVery-strangeCasing"
                    in
                    Expect.equal word (toSnake word)
            , test "it cannot convert multiple words" <|
                \_ ->
                    let
                        words =
                            "BlueCharlie RedFoxtrot WhiskeyAlpha"
                    in
                    Expect.equal words (toSnake words)
            ]
        ]
