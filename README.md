# elm-csv

Parse CSV files according to [RFC 4180](https://tools.ietf.org/html/rfc4180) (including values with quoted commas).

## Quick Start

Suppose you have a file `star-wars-quotes.csv` with the following content:

```csv
quote,character
"Why, hello there",Obi-Wan Kenobi
"General Kenobi.",General Grievous
```

As an Elm string literal, this would be represented as:
```elm
content = 
  "quote,character\n\"Why, hello there\",Obi-Wan Kenobi\n\"General Kenobi.\",General Grievous"
```

Calling `Csv.parseCsv` will produce a record with `{ headers: String, rows: String }` where `headers` is `[ "quote", "character" ]` and `rows` is a list of list of strings, properly escaped from their CSV representation.
