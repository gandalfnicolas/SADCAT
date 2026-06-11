# Prepare SADCAT Dictionaries for Quanteda Matching

Transforms `All.steps_Dictionaries` into a quanteda dictionary object.
The result is cached in a package-level environment so it only needs to
be computed once per R session.

## Usage

``` r
prepare_sadcat_dictionaries(
  pre_dictionaries = All.steps_Dictionaries,
  cache = TRUE
)
```

## Arguments

- pre_dictionaries:

  The raw SADCAT dictionary data (default: All.steps_Dictionaries)

- cache:

  Logical. Cache result for session? (default TRUE)

## Value

A quanteda::dictionary object with ~33 categories
