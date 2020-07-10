module Csv.Test exposing (suite)

import Csv
import Expect
import Test exposing (Test, describe, only, test)


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
                , test "Parsing with carriage returns" <|
                    \_ ->
                        Csv.parseRows ",The number of iterations required in ADMM typically drops dramatically during the first couple of iterations of the primal dual method.,\"In particular, it dramatically decreases the amount of ADMM iterations necessary in the first couple of iterations of the primal-dual method.\"\u{000D}\nother,fact update because they now propose two methods,We have proposed an efficient distributed primal-dual interior-point method for loosely coupled problems using ADMM (Algorithm [REF]).,We have proposed two efficient distributed primal-dual interior-point method for loosely coupled problems using ADMM (Algorithm [REF] and Algorithm [REF]).\u{000D}\n"
                            |> Expect.equal
                                (Ok
                                    [ [ ""
                                      , "The number of iterations required in ADMM typically drops dramatically during the first couple of iterations of the primal dual method."
                                      , "In particular, it dramatically decreases the amount of ADMM iterations necessary in the first couple of iterations of the primal-dual method."
                                      ]
                                    , [ "other"
                                      , "fact update because they now propose two methods"
                                      , "We have proposed an efficient distributed primal-dual interior-point method for loosely coupled problems using ADMM (Algorithm [REF])."
                                      , "We have proposed two efficient distributed primal-dual interior-point method for loosely coupled problems using ADMM (Algorithm [REF] and Algorithm [REF])."
                                      ]
                                    ]
                                )
                , test "parsing with carriage returns (real world bug)" <|
                    \_ ->
                        Csv.parseRows ",1406.2192-v1-11-2,,We illustrate the method on a numerical example in Section [REF].,\u{000D}\n,1406.2192-v1-15-11,,The latter are briefly reviewed in the next section.,\u{000D}\n,1406.2192-v1-17-3,,Next we briefly review how this is done within a primal-dual framework.,\u{000D}\n,1406.2192-v1-19-0,,First notice that for any [MATH] and [MATH] that satisfy [EQUATION] we have [MATH] and [MATH] for all [MATH].,\u{000D}\n"
                            |> Expect.equal
                                (Ok
                                    [ [ ""
                                      , "1406.2192-v1-11-2"
                                      , ""
                                      , "We illustrate the method on a numerical example in Section [REF]."
                                      , ""
                                      ]
                                    , [ ""
                                      , "1406.2192-v1-15-11"
                                      , ""
                                      , "The latter are briefly reviewed in the next section."
                                      , ""
                                      ]
                                    , [ ""
                                      , "1406.2192-v1-17-3"
                                      , ""
                                      , "Next we briefly review how this is done within a primal-dual framework."
                                      , ""
                                      ]
                                    , [ ""
                                      , "1406.2192-v1-19-0"
                                      , ""
                                      , "First notice that for any [MATH] and [MATH] that satisfy [EQUATION] we have [MATH] and [MATH] for all [MATH]."
                                      , ""
                                      ]
                                    ]
                                )
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
