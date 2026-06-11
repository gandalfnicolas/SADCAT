# Raw SOCATS social category dictionary

Source data used by
[`prepare_socats_dictionaries()`](https://gandalfnicolas.github.io/SADCAT/reference/prepare_socats_dictionaries.md)
to build the quanteda dictionary for SOCATS social category matching.

## Usage

``` r
SOCATS_Dictionaries
```

## Format

A data frame with SOCATS category labels and raw dictionary terms. Key
column groups:

- `word`: Raw social category term

- One binary indicator column per SOCATS category used by the matcher

- Category families spanning race/ethnicity, age, sexuality, gender,
  jobs, and geography
