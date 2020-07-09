module Csv.Test exposing (suite)

import Csv
import Expect
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "parsing and generating csv files"
        [ describe "Parsing csv files as rows"
            [ describe "Csv.parseRows"
                [ test "empty" <|
                    \_ ->
                        Csv.parseRows "" |> Expect.equal (Ok [])
                , test "one char" <|
                    \_ ->
                        Csv.parseRows "a" |> Expect.equal (Ok [ [ "a" ] ])
                , test "two char" <|
                    \_ ->
                        Csv.parseRows "ab" |> Expect.equal (Ok [ [ "ab" ] ])
                , test "two values" <|
                    \_ ->
                        Csv.parseRows "a,b" |> Expect.equal (Ok [ [ "a", "b" ] ])
                , test "many values" <|
                    \_ ->
                        Csv.parseRows "a,b,c,d,\"\"\"quoted\"\",\""
                            |> Expect.equal (Ok [ [ "a", "b", "c", "d", "\"quoted\"," ] ])
                , test "values with commas" <|
                    \_ ->
                        Csv.parseRows "simplification,word usage,removes the clause about keeping the system in a pure state,0803.2581-v1-3-5,0803.2581-v2-3-4,\"The entanglement with an environment provokes the loss of coherence in the system -coherence which is necessary to keep the system in a pure state and display typical quantum effects, such as diffraction-like structures, for instance.\",\"When we \"\"look\"\" at the reduced system dynamics, this entanglement translates into a partial or even total loss of the system coherence.\""
                            |> Expect.equal
                                (Ok
                                    [ [ "simplification"
                                      , "word usage"
                                      , "removes the clause about keeping the system in a pure state"
                                      , "0803.2581-v1-3-5"
                                      , "0803.2581-v2-3-4"
                                      , "The entanglement with an environment provokes the loss of coherence in the system -coherence which is necessary to keep the system in a pure state and display typical quantum effects, such as diffraction-like structures, for instance."
                                      , "When we \"look\" at the reduced system dynamics, this entanglement translates into a partial or even total loss of the system coherence."
                                      ]
                                    ]
                                )
                , test "real-world example" <|
                    \_ ->
                        Csv.parseRows "\"a,\",b" |> Expect.equal (Ok [ [ "a,", "b" ] ])
                , test "quotes and commas" <|
                    \_ ->
                        Csv.parseRows "\"start: \"\"quote\"\" comma, end.\""
                            |> Expect.equal (Ok [ [ "start: \"quote\" comma, end." ] ])
                , test "no quotes" <|
                    \_ ->
                        Csv.parseRows "start: end."
                            |> Expect.equal (Ok [ [ "start: end." ] ])
                , test "comma" <|
                    \_ ->
                        Csv.parseRows "start, end"
                            |> Expect.equal (Ok [ [ "start", " end" ] ])
                , test "newlines" <|
                    \_ ->
                        Csv.parseRows "start\nend"
                            |> Expect.equal (Ok [ [ "start" ], [ "end" ] ])
                , test "escaped with newlines" <|
                    \_ ->
                        Csv.parseRows "\"start\"\nend"
                            |> Expect.equal (Ok [ [ "start" ], [ "end" ] ])
                , test "escaped with newlines on both" <|
                    \_ ->
                        Csv.parseRows "\"start\"\n\"end\""
                            |> Expect.equal (Ok [ [ "start" ], [ "end" ] ])
                , test "quoted newlines" <|
                    \_ ->
                        Csv.parseRows "\"start\nend\""
                            |> Expect.equal (Ok [ [ "start\nend" ] ])
                , test "newlines in quotes" <|
                    \_ ->
                        Csv.parseRows "\"st\"\"art\nend\"\"\""
                            |> Expect.equal (Ok [ [ "st\"art\nend\"" ] ])
                , test "minimal" <|
                    \_ ->
                        Csv.parseRows "\"start: \"\"quote\"\" comma, end.\"\n"
                            |> Expect.equal (Ok [ [ "start: \"quote\" comma, end." ] ])
                , test "start with comma" <|
                    \_ ->
                        Csv.parseRows ",1,2"
                            |> Expect.equal (Ok [ [ "", "1", "2" ] ])
                , test "start with comma (real world)" <|
                    \_ ->
                        Csv.parseRows ",1410.4028-v1-3-6,1410.4028-v1-3-6,Hello there.,Hello world!"
                            |> Expect.equal (Ok [ [ "", "1410.4028-v1-3-6", "1410.4028-v1-3-6", "Hello there.", "Hello world!" ] ])
                , test "demo not working" <|
                    \_ ->
                        Csv.parseRows "simplification,word usage,removes the clause about keeping the system in a pure state,0803.2581-v1-3-5,0803.2581-v2-3-4,\"The world.\",\"When \"\"we\"\" look, this causes pain.\""
                            |> Expect.equal (Ok [ [ "simplification", "word usage", "removes the clause about keeping the system in a pure state", "0803.2581-v1-3-5", "0803.2581-v2-3-4", "The world.", "When \"we\" look, this causes pain." ] ])
                ]
            ]
        , describe "Escaping strings to make csv files"
            [ describe "Csv.escapeRows"
                [ test "empty" <|
                    \_ ->
                        Csv.escapeRows [] |> Expect.equal ""
                , test "empty row" <|
                    \_ ->
                        Csv.escapeRows [ [] ] |> Expect.equal ""
                , test "one row with an empty value" <|
                    \_ ->
                        Csv.escapeRows [ [ "" ] ] |> Expect.equal ""
                , test "one row with two empty values" <|
                    \_ ->
                        Csv.escapeRows [ [ "", "" ] ] |> Expect.equal ","
                , test "value with a comma" <|
                    \_ ->
                        Csv.escapeRows [ [ "hello, world", "" ] ]
                            |> Expect.equal "\"hello, world\","
                , test "value with a quote mark" <|
                    \_ ->
                        Csv.escapeRows [ [ "\"General Kenobi\"", "" ] ]
                            |> Expect.equal "\"\"\"General Kenobi\"\"\","
                , test "value with a quote mark and comma" <|
                    \_ ->
                        Csv.escapeRows [ [ "\"General Kenobi\", said Grievous", "" ] ]
                            |> Expect.equal "\"\"\"General Kenobi\"\", said Grievous\","
                , test "values with a quote mark and comma" <|
                    \_ ->
                        Csv.escapeRows [ [ "\"Hello there\".", "\"General Kenobi\", said Grievous" ] ]
                            |> Expect.equal "\"\"\"Hello there\"\".\",\"\"\"General Kenobi\"\", said Grievous\""
                ]
            ]
        ]
