module TitleTest exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Main exposing (..)
import Test exposing (..)


suite : Test
suite =
    describe "Title"
        [ describe "toTitle"
            [ test "it can convert camel case" <|
                \_ ->
                    let
                        camel =
                            "someThingVeryCool"

                        expected =
                            "SomeThingVeryCool"
                    in
                    Expect.equal expected (toTitle camel)
            , test "it can convert kebab case" <|
                \_ ->
                    let
                        kebab =
                            "some-thing-very-cool"

                        expected =
                            "SomeThingVeryCool"
                    in
                    Expect.equal expected (toTitle kebab)
            , test "it can convert snake case" <|
                \_ ->
                    let
                        snake =
                            "simple_http_request"

                        expected =
                            "SimpleHttpRequest"
                    in
                    Expect.equal expected (toTitle snake)
            , test "it can convert multiple words w/ consistent casing" <|
                \_ ->
                    let
                        words =
                            "redAlpha blueFoxtrot whiskyTango"

                        expected =
                            "RedAlpha BlueFoxtrot WhiskyTango"
                    in
                    Expect.equal expected (toTitle words)
            ]
        ]
