module Csv exposing (Csv, escapeCsv, escapeRows, parseCsv, parseRows)

import Helper exposing (State(..), escape, flipRows, nextState)


type alias Csv =
    { headers : List String
    , rows : List (List String)
    }


escapeCsv : Csv -> String
escapeCsv { headers, rows } =
    escapeRows (headers :: rows)


escapeRows : List (List String) -> String
escapeRows rows =
    String.join
        "\n"
        (List.map
            (List.map escape >> String.join ",")
            rows
        )


parseCsv : String -> Result String Csv
parseCsv str =
    case parseRows str of
        Err err ->
            Err err

        Ok (headers :: rows) ->
            Ok { headers = headers, rows = rows }

        Ok [] ->
            Err "no rows found in .csv file"


parseRows : String -> Result String (List (List String))
parseRows str =
    case nextState Initial str "" [] [] of
        Err err ->
            Err err

        Ok rows ->
            Ok (flipRows rows)
