# Prepare SOCATS Dictionaries for Quanteda Matching

Transforms `SOCATS_Dictionaries` into a quanteda dictionary object for
social category matching. The result is cached in a package-level
environment so it only needs to be computed once per R session.
Preprocessing includes UTF-8 encoding, lowercasing, dash-to-space
conversion, lemmatization, removal of ending Ss, and punctuation/symbol
removal.

## Usage

``` r
prepare_socats_dictionaries(socats_data = SOCATS_Dictionaries, cache = TRUE)
```

## Arguments

- socats_data:

  The raw SOCATS dictionary data frame (default: SOCATS_Dictionaries)

- cache:

  Logical. Cache result for session? (default TRUE)

## Value

A quanteda::dictionary object with 37 social category entries
