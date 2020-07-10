module Csv exposing
    ( parseRows
    , escapeRows
    )

{-| Parse CSV files according to RFC 4180.


# Parsing

@docs parseRows


# Generating

@docs escapeRows

-}

import Helper exposing (State(..), escape, flipRows, nextState)


{-| Escape a list of CSV rows and produce a single string (that can be written to a file)

    escapeRows [ [ "start" ], [ "\"end\"" ] ] --> "start\n\"\"\"end\"\"\""

-}
escapeRows : List (List String) -> String
escapeRows rows =
    String.join
        "\n"
        (List.map
            (List.map escape >> String.join ",")
            rows
        )


{-| Parse a string reprenting the contents of a CSV file. If the string is invalid CSV, returns Err with the CSV that was already parsed when the error was encountered.

    parseRows "hello,world\n- a programmer" --> Ok [ [ "hello", "world" ], ["- a programmer"] ]

-}
parseRows : String -> Result String (List (List String))
parseRows str =
    case nextState Initial str "" [] [] of
        Err err ->
            Err err

        Ok rows ->
            Ok (flipRows rows)
