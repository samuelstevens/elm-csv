module Helper exposing (State(..), escape, flipRows, nextState)


type State
    = Initial
    | Field
    | CheckDoubleQuote
    | Escaped
    | NonEscaped
    | Invalid


flipRowAndText : List String -> List String
flipRowAndText row =
    row
        |> List.reverse
        |> List.map String.reverse


flipRows : List (List String) -> List (List String)
flipRows rows =
    rows
        |> List.reverse
        |> List.map flipRowAndText


nextState : State -> String -> String -> List String -> List (List String) -> Result String (List (List String))
nextState state remaining field row finished =
    case state of
        Initial ->
            nextState Field remaining "" [] []

        Field ->
            case String.uncons remaining of
                Just ( ',', rest ) ->
                    nextState Field rest "" ("" :: row) finished

                Just ( '"', rest ) ->
                    nextState Escaped rest "" row finished

                Just ( ch, rest ) ->
                    nextState NonEscaped rest (String.fromChar ch) row finished

                Nothing ->
                    Ok finished

        NonEscaped ->
            case String.uncons remaining of
                Just ( ch, rest ) ->
                    case ch of
                        ',' ->
                            nextState Field rest "" (field :: row) finished

                        '\n' ->
                            nextState Field rest "" [] ((field :: row) :: finished)

                        '\u{000D}' ->
                            nextState Field rest "" [] ((field :: row) :: finished)

                        _ ->
                            nextState NonEscaped rest (String.cons ch field) row finished

                Nothing ->
                    Ok ((field :: row) :: finished)

        CheckDoubleQuote ->
            case String.uncons remaining of
                Just ( ch, rest ) ->
                    case ch of
                        '"' ->
                            nextState Escaped rest (String.cons '"' field) row finished

                        ',' ->
                            nextState Field rest "" (field :: row) finished

                        '\n' ->
                            nextState Field rest "" [] ((field :: row) :: finished)

                        '\u{000D}' ->
                            nextState Field rest "" [] ((field :: row) :: finished)

                        _ ->
                            nextState Invalid rest "" (field :: row) finished

                Nothing ->
                    Ok ((field :: row) :: finished)

        Escaped ->
            case String.uncons remaining of
                Just ( ch, rest ) ->
                    case ch of
                        ',' ->
                            nextState Escaped rest (String.cons ',' field) row finished

                        '"' ->
                            nextState CheckDoubleQuote rest field row finished

                        _ ->
                            nextState Escaped rest (String.cons ch field) row finished

                Nothing ->
                    nextState Invalid "" "" (field :: row) finished

        Invalid ->
            Err remaining


escapeQuotes : String -> String
escapeQuotes str =
    String.replace "\"" "\"\"" str


escape : String -> String
escape str =
    let
        escaped =
            escapeQuotes str
    in
    if String.contains "," escaped || String.contains "\"" escaped then
        "\"" ++ escaped ++ "\""

    else
        str
