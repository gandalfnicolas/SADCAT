# Match Text Against SADCAT and/or SOCATS Dictionaries

Match text responses against SADCAT stereotype dictionaries and/or
SOCATS social category dictionaries. Tokenizes response text, matches
against dictionaries, and computes binary indicators, percentages,
direction scores, and per-dimension valence/direction columns.

## Usage

``` r
match_dictionaries(
  data,
  text_col = "tv3",
  response_col = NULL,
  valence_col = "ValenceYesNA",
  valence_nona_col = "ValenceNoNA",
  sadcat_dict = NULL,
  socats_dict = NULL,
  socats = FALSE,
  keep_intermediate = FALSE
)
```

## Arguments

- data:

  A data.frame with preprocessed text and valence scores

- text_col:

  Column with singularized text to match (default "tv3")

- response_col:

  Column used for NA-gating (default: same as text_col). Only needed if
  your NA-indicator column differs from text_col.

- valence_col:

  Name of NA-gated combined valence column (default "ValenceYesNA")

- valence_nona_col:

  Name of zero-imputed combined valence column (default "ValenceNoNA")

- sadcat_dict:

  Pre-computed SADCAT quanteda dictionary. If NULL and `sadcat = TRUE`
  (default), calls
  [`prepare_sadcat_dictionaries()`](https://gandalfnicolas.github.io/SADCAT/reference/prepare_sadcat_dictionaries.md).
  Set to FALSE to skip SADCAT matching entirely.

- socats_dict:

  Pre-computed SOCATS quanteda dictionary. If NULL and `socats = TRUE`,
  calls
  [`prepare_socats_dictionaries()`](https://gandalfnicolas.github.io/SADCAT/reference/prepare_socats_dictionaries.md).

- socats:

  Logical. Match against SOCATS dictionaries? (default FALSE)

- keep_intermediate:

  Logical. If `FALSE` (default), return compact SADCAT outputs with
  informative names: `{Dim}_prevalence`, `{Dim}_valence`,
  `{Dim}_direction`, `{Dim}_valenceNoNA`, `{Dim}_directionNoNA`, and
  `NoMatch`. If `TRUE`, keep legacy SADCAT intermediate columns as well.

## Value

The input data with dictionary-derived columns appended. By default,
SADCAT outputs are compact. For each SADCAT dimension `{Dim}`, three
valence columns are produced:

- `{Dim}_Valence` (default for downstream means): NA when the dimension
  is not tagged in the response; otherwise the global `ValenceNoNA` (0
  if no sentiment words matched, else the signed negation-aware mean).
  Use `mean(., na.rm = TRUE)` to get the average valence among tagged
  responses, where sentiment-less tagged responses contribute 0.

- `{Dim}_valenceStrictNA`: NA whenever either the dimension is not
  tagged OR the global `ValenceYesNA` is itself NA. Strictly NA-gated on
  both axes.

- `{Dim}_valenceNoNA`: 0 whenever either the dimension is not tagged OR
  the global sentiment is NA. Strictly zero-imputed on both axes;
  interpretable as a prevalence-weighted valence over the full response
  set.
