module UtilsTest exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Main exposing (..)
import Test exposing (..)
import Utils exposing (..)


suite : Test
suite =
    describe "Utils"
        [ describe "toTitleWithSeparator"
            [ test "converts all words to title case, converts from given separator" <|
                \_ ->
                    let
                        input =
                            "I cant-believe its not_butter"

                        expected =
                            "I CantBelieve Its Not_butter"
                    in
                    Expect.equal expected (toTitleWithSeparator '-' input)
            , test "it converts all words that are separator by input" <|
                \_ ->
                    let
                        input =
                            "whisky_tango alpha_bravo omega_foxtrot"

                        expected =
                            "WhiskyTango AlphaBravo OmegaFoxtrot"
                    in
                    Expect.equal expected (toTitleWithSeparator '_' input)
            ]
        , describe "fromTitleWithSeparator"
            [ test "converts all title case words to words separated by given input" <|
                \_ ->
                    let
                        input =
                            "WhiskyTango AlphaBravo OmegaFoxtrot"

                        expected =
                            "whisky_tango alpha_bravo omega_foxtrot"
                    in
                    Expect.equal expected (fromTitleWithSeparator '_' input)
            ]
        , describe "splitOn"
            [ test "it splits a string on a given Char" <|
                \_ ->
                    let
                        input =
                            "alpha, bravo, tango foxtrot, omega"

                        expected =
                            [ "alpha", " bravo", " tango foxtrot", " omega" ]
                    in
                    Expect.equal expected (splitOn ',' input)
            ]
        , describe "replaceAll" <|
            [ test "it takes a tuple of chars and all instances of one with the other" <|
                \_ ->
                    let
                        input =
                            "The quick brown dog"

                        expected =
                            "The quick brawn dag"
                    in
                    Expect.equal expected (replaceAll ( 'o', 'a' ) input)
            ]
        , describe "mapWords"
            [ test "it takes a function to be applied to every word in a string" <|
                \_ ->
                    let
                        input =
                            "The quick brown dog"

                        expected =
                            "e ick own g"

                        transformation =
                            String.dropLeft 2
                    in
                    Expect.equal expected (mapWords transformation input)
            ]
        , describe "mapFirst"
            [ test "it takes a function to be applied to the first char in a string" <|
                \_ ->
                    let
                        input =
                            "carrot juice"

                        transformation =
                            String.pad 3 '_'

                        expected =
                            "_c_arrot juice"
                    in
                    Expect.equal expected (mapFirst transformation input)
            ]
        , describe "isCamel"
            [ test "it returns True if a string is camelCased" <|
                \_ ->
                    let
                        input =
                            "someCamelCasedWord"
                    in
                    Expect.equal True (isCamel input)
            , test "it returns False if a string is not camel cased" <|
                \_ ->
                    let
                        input =
                            "TitleCased"
                    in
                    Expect.equal False (isCamel input)
            , test "does not ignore white spaces" <|
                \_ ->
                    let
                        input =
                            "someWord anotherWord andAFinalWord"
                    in
                    Expect.equal False (isCamel input)
            , test "does not ignore commas or any other invalid characters" <|
                \_ ->
                    let
                        input =
                            "someWord,anotherWord"
                    in
                    Expect.equal False (isCamel input)
            ]
        , describe "isTitle"
            [ test "it returns True if a string is TitleCased" <|
                \_ ->
                    let
                        input =
                            "SomeWhereOverTheRainbow"
                    in
                    Expect.equal True (isTitle input)
            , test "it returns False if string is any other case" <|
                \_ ->
                    let
                        words =
                            [ "snake_case", "camelCase", "kebab-case", "SCREAMING_SNAKE" ]
                    in
                    words
                        |> List.any isTitle
                        |> Expect.equal False
            , test "it does not ignore white spaces" <|
                \_ ->
                    let
                        input =
                            "In West Philadephia born and raised"
                    in
                    Expect.equal False (isTitle input)
            ]
        , describe "isSnake"
            [ test "it returns True if a string is snake_case" <|
                \_ ->
                    let
                        input =
                            "some_where_over_the_rainbow"
                    in
                    Expect.equal True (isSnake input)
            , test "it returns true if a string is SCREAMING_SNAKE case" <|
                \_ ->
                    let
                        input =
                            "SOME_WHERE_OVER_THE_RAINBOW"
                    in
                    Expect.equal True (isSnake input)
            , test "it returns False if string is any other case" <|
                \_ ->
                    let
                        words =
                            [ "TitleCase", "camelCase", "kebab-case", "word" ]
                    in
                    words
                        |> List.any isSnake
                        |> Expect.equal False
            , test "it does not ignore white spaces" <|
                \_ ->
                    let
                        input =
                            "some_thing something_else"
                    in
                    Expect.equal False (isSnake input)
            ]
        , describe "isKebab"
            [ test "it returns True if a string is camelCased" <|
                \_ ->
                    let
                        input =
                            "some-where-over-the-rainbow"
                    in
                    Expect.equal True (isKebab input)
            , test "it returns False if string is any other case" <|
                \_ ->
                    let
                        words =
                            [ "TitleCase", "snake_case", "camelCase", "SCREAMING_SNAKE" ]
                    in
                    words
                        |> List.any isKebab
                        |> Expect.equal False
            , test "it does not ignore white spaces" <|
                \_ ->
                    let
                        input =
                            "in-west philadelphia born and-raised"
                    in
                    Expect.equal False (isKebab input)
            ]
        ]
